result = True

try:
    strin = raw_input()

    if strin:
        result = False
    
except EOFError:
    potato = 5

if not result:
    print "TEST FAILED"
else:
    print "test passed"
