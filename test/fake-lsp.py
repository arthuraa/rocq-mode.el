#!/usr/bin/env python3
"""Minimal LSP stdio server standing in for coq-lsp in the test suite.

It logs every method it receives to the file given as its argument, one
per line with a short summary of the parameters, answers `initialize',
`shutdown' and `proof/goals', and sends a `$/coq/fileProgress'
notification whenever a document is opened.  Set FAKE_IGNORE_SHUTDOWN to
simulate a busy server that never answers `shutdown'.
"""
import json, os, sys
log = open(sys.argv[1], "a")
inp, out = sys.stdin.buffer, sys.stdout.buffer

def send(obj):
    data = json.dumps(obj).encode()
    out.write(b"Content-Length: %d\r\n\r\n" % len(data)); out.write(data); out.flush()

def summarize(method, params):
    if method == "initialize":
        return {"rootUri": params.get("rootUri"), "workspaceFolders": params.get("workspaceFolders")}
    if method == "workspace/didChangeWorkspaceFolders":
        return params.get("event")
    if method.startswith("textDocument/"):
        return params.get("textDocument", {}).get("uri")
    return None

while True:
    headers = {}
    line = inp.readline()
    if not line:
        break
    while line not in (b"\r\n", b"\n"):
        k, _, v = line.decode().partition(":")
        headers[k.strip().lower()] = v.strip()
        line = inp.readline()
    msg = json.loads(inp.read(int(headers["content-length"])))
    method = msg.get("method")
    if method:
        log.write("%s %s\n" % (method, json.dumps(summarize(method, msg.get("params") or {}), sort_keys=True))); log.flush()
    if method == "textDocument/didOpen":
        td = msg["params"]["textDocument"]
        send({"jsonrpc": "2.0", "method": "$/coq/fileProgress",
              "params": {"textDocument": {"uri": td["uri"], "version": td["version"]},
                         "processing": [{"range": {"start": {"line": 0, "character": 0},
                                                   "end": {"line": 0, "character": 10}},
                                         "kind": 1}]}})
    if method == "exit":
        break
    if "id" in msg and method:
        result = None
        if method == "shutdown" and os.environ.get("FAKE_IGNORE_SHUTDOWN"):
            continue  # simulate a busy server that never answers
        if method == "proof/goals":
            # The answer depends on the line asked about, so that tests
            # can see goals come and go: lines 0-2 have two focused
            # goals, line 3 one with an error, line 4 none but an
            # unfocused one, and later lines are outside a proof.
            td = msg["params"]["textDocument"]
            line = msg["params"]["position"]["line"]
            first = {"info": {"evar": ["Ser_Evar", 1], "name": None},
                     "hyps": [{"names": ["n"], "ty": "nat"}], "ty": "n = n"}
            second = {"info": {"evar": ["Ser_Evar", 2], "name": "second"},
                      "hyps": [], "ty": "True /\\\n  True"}
            unfocused = {"info": {"evar": ["Ser_Evar", 3], "name": None},
                         "hyps": [], "ty": "False"}
            shelved = {"info": {"evar": ["Ser_Evar", 4], "name": None},
                       "hyps": [], "ty": "Shelved"}
            goals = {"goals": [first, second] if line < 3 else [second] if line == 3 else [],
                     "stack": [[[], [unfocused]]], "shelf": [shelved], "given_up": []}
            result = {"textDocument": {"uri": td["uri"], "version": 0},
                      "position": msg["params"]["position"],
                      "messages": [{"level": 2, "text": "a warning"}], "range": None}
            if line < 5:
                result["goals"] = goals
            if line == 3:
                result["error"] = "an error"
        if method == "initialize":
            result = {"capabilities": {"workspace": {"workspaceFolders": {"supported": True, "changeNotifications": True}}},
                      "serverInfo": {"name": "fake"}}
        send({"jsonrpc": "2.0", "id": msg["id"], "result": result})
