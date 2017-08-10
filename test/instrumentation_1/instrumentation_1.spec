{
    "ltl": "G (p && q && r)",
    "pa": [
        {
            "name": "p",
            "default": false,
            "expr": "fp",
            "span": ["_begin", "_end"],
            "params": ["g1"]
        },
        {
            "name": "r",
            "default": false,
            "expr": "fr",
            "span": ["l1", "l2"],
            "params": ["t1::v1", "t1::v2", "t1::v3"]
        },
        {
            "name": "q",
            "default": false,
            "expr": "fq",
            "span": ["lg1", "lg2"],
            "params": ["g1", "g2", "t1::v1", "t2::v4"]
        }
    ]
}
