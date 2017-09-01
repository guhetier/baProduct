{
    "ltl": "G (red1 || red2)",
    "pa": [
        {
            "name": "red1",
            "default": true,
            "expr": "isRed1",
            "span": ["b1", "e1"],
            "params": ["signal1::status"]
        },
        {
            "name": "red2",
            "default": true,
            "expr": "isRed2",
            "span": ["b2", "e2"],
            "params": ["signal2::status"]
        }
    ]
}
