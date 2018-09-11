

def AND(x1, x2):
    w1, w2, theta = 0.5, 0.5, 0.7

    return 0 if (x1*w1 + x2*w2 < theta) else 1


if __name__ == "__main__":
    def test_equal(title, cond, ans):
        print( "%s: test:\"%s\" .. %s " % ("OK" if cond == ans else "NG", title, cond))
        return cond == ans

    tests = [ test_equal( "AND 0 0", AND(0,0), 0),
              test_equal( "AND 0 1", AND(0,1), 0),
              test_equal( "AND 1 0", AND(1,0), 0),
              test_equal( "AND 1 1", AND(1,1), 1)
    ]
    print ( "%d tests: OK %d   NG %d" % (len(tests), len([i for i in tests if i ]), len([i for i in tests if not i])))

