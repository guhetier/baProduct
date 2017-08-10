{
    "ltl": "F r -> (p -> (!r U (q && !r))) U r",
    "pa": [
        {
            "name": "p",
            "default": false,
            "expr": "fp",
            "span": ["_begin", "_end"],
            "params": []
        },
        {
            "name": "r",
            "default": false,
            "expr": "fr",
            "span": ["_begin", "_end"],
            "params": []
        },
        {
            "name": "q",
            "default": false,
            "expr": "fq",
            "span": ["_begin", "_end"],
            "params": []
        }
    ]
}
