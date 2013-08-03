(def agent0 (agent 0)) ; (agent initial-state & options)
(def agent1 (agent 0)) ; options are :meta and :validator
(def agent2 (agent 0))

; use send for CPU intensive actions which don't block
(send agent0 + 1000)

; send-off can handle potential blockings by growing the thread pool (unlike send)
(send-off agent1 + 10)


; await an agent, this action will block until thread is ready
(await agent0)

; await-for waits for specific number of msecs
(await-for 1000 agent1)

; if something happens, it's possible to check...
(agent-errors agent2) ; returns nil, so everything is fine

; if there were an error...
(clear-agent-errors agent2)