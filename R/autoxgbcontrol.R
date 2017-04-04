# Default MBO control object uses 80 iterations or one hour runtime
autoxgbcontrol = makeMBOControl()
autoxgbcontrol = setMBOControlTermination(autoxgbcontrol, iters = 80L, time.budget = 3600L)
