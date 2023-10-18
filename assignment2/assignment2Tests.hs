import Assignment2
import Control.Exception (assert)
import Data.Word ( Word8 )
import Data.Ratio ( (%))

main :: IO()
main = do
    print "1.-------------------"
    print (assert (isAP []) "Passed")
    print (assert (isAP [2]) "Passed")
    print (assert (isAP [-22, -385]) "Passed")
    print (assert (isAP [-22, -385, -385+(-385+22)]) "Passed")
    print (assert (isAP [2,19,36,53,70,87]) "Passed")
    print (assert (not (isAP [2,19,36,52,69,86])) "Passed")
    print (assert (isAP (reverse [2,19,36,53,70,87])) "Passed")

    print "2.-------------------"

    print (assert (not (targetSum 100 [])) "Passed")
    print (assert (not (targetSum 100 [50])) "Passed")
    print (assert (targetSum 10 [4,6]) "Passed")
    print (assert (targetSum 10 [4,5,5,6]) "Passed")
    print (assert (not (targetSum 20 [1,2,3,4,5,6,7,8,9,10])) "Passed")
    print (assert (targetSum 20  [10,9,8,6,7,8,9,10] ) "Passed")
    print (assert (targetSum 20  [1,9,8,-10,7,8,9,30] ) "Passed")

    print "3.-------------------"
    print (assert (incr [] == [1]) "Passed")
    print (assert (incr [0] == [1]) "Passed")
    print (assert (incr [254] == [255]) "Passed")
    print (assert (incr [255] == [1, 0]) "Passed")
    print (assert (incr [255,22,23,255,255] == [255,22,24,0,0]) "Passed")
    print (assert (incr [255,255,255,255,255] == [1,0,0,0,0,0]) "Passed")

    print "4.--------------------"
    print (assert (twosVal [False, False, False, False, False, False, False, False] == 0) "Passed")
    print (assert (twosVal [False, False, False, False, False, False, False, True] == 1) "Passed")
    print (assert (twosVal [False, True, True, False, True, False, False, False] == 104) "Passed")
    print (assert (twosVal [False, True, True, True, True, True, True, True] == 127) "Passed")
    print (assert (twosVal [True, False, False, False, False, False, False, False] == -128) "Passed")
    print (assert (twosVal [True, False, False, True, True, False, False, False] == -104) "Passed")
    print (assert (twosVal [True, True, True, False, True, False, False, False] == -24) "Passed")
    print (assert (twosVal [True, True, True, True, True, True, True, True] == -1) "Passed")

    print (assert (twosRep 0 ==  [False, False, False, False, False, False, False, False]) "Passed")
    print (assert (twosRep 1 == [False, False, False, False, False, False, False, True]) "Passed")
    print (assert (twosRep 104 == [False, True, True, False, True, False, False, False]) "Passed")
    print (assert (twosRep 127 == [False, True, True, True, True, True, True, True]) "Passed")
    print (assert (twosRep (-128) == [True, False, False, False, False, False, False, False]) "Passed")
    print (assert (twosRep (-104) == [True, False, False, True, True, False, False, False]) "Passed")
    print (assert (twosRep (-24) == [True, True, True, False, True, False, False, False]) "Passed")
    print (assert (twosRep (-1) == [True, True, True, True, True, True, True, True]) "Passed")

    print "5.--------------------"
    print (assert (computeRat [1,2,1,4,2] == 42%31) "Passed")
    print (assert (computeRat [1,4,5] == 26%21) "Passed")
    print (assert (computeRat [-1,1,1,1,1] == (-2)%5) "Passed")
    print (assert (computeRat [0,1,1,1,1] == 3%5) "Passed")
    print (assert (computeRat [0,1,1,2] == 3%5) "Passed")
    print (assert (computeRat [-2,1,3,5]== (-26)%21) "Passed")

    

