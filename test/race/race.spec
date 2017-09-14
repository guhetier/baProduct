{
    "ltl": "G ((e2 && !e1) -> FG(w1))",
    "pa": [
        {
            "name": "e1",
            "default": false,
            "expr": "p_ended",
            "span": ["r1b", "r1e"],
            "params": ["player1::f"]
        },
        {
            "name": "e2",
            "default": false,
            "expr": "p_ended",
            "span": ["r2b", "r2e"],
            "params": ["player2::f"]
        },
        {
            "name": "w1",
            "default": true,
            "expr": "w1",
            "span": ["_begin", "_end"],
            "params": ["score"]
        }
    ]
}
