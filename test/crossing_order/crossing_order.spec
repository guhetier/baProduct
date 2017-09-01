{
    "ltl": "G (green1 -> (!red1 U orange1))",
    "pa": [
        {
            "name": "green1",
            "default": false,
            "expr": "isGreen1",
            "span": ["b1", "e1"],
            "params": ["signal1::status"]
        },
        {
            "name": "orange1",
            "default": false,
            "expr": "isOrange1",
            "span": ["b1", "e1"],
            "params": ["signal1::status"]
        },
        {
            "name": "red1",
            "default": false,
            "expr": "isRed1",
            "span": ["b1", "e1"],
            "params": ["signal1::status"]
        }

    ]
}
