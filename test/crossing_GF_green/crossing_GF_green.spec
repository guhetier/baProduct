{
    "ltl": "(G F green1) && (G F green2)",
    "pa": [
        {
            "name": "green1",
            "default": false,
            "expr": "isGreen1",
            "span": ["b1", "e1"],
            "params": ["signal1::status"]
        },
        {
            "name": "green2",
            "default": false,
            "expr": "isGreen2",
            "span": ["b2", "e2"],
            "params": ["signal2::status"]
        }
    ]
}
