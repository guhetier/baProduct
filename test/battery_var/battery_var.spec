{
    "ltl": "G(! (p1 && p2))",
    "pa": [
        {
            "name": "p1",
            "default": false,
            "expr": "fp1",
            "span": ["t1", "e1"],
            "params": ["thread1::energy_stored"]
        },
        {
            "name": "p2",
            "default": false,
            "expr": "fp2",
            "span": ["t2", "e2"],
            "params": ["thread2::energy_stored"]
        }
    ]
}
