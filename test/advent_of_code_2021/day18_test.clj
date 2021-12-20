(ns advent-of-code-2021.day18-test
  (:require [clojure.test :refer :all]
            [advent_of_code_2021.day18 :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(deftest day18
  (testing "day 18"
    (is (= [4 "[9,8]"] (find-pair-of-regular-numbers-nested-inside-four-pairs "[[[[[9,8],1],2],3],4]")))
    (is (= [12 "[3,2]"] (find-pair-of-regular-numbers-nested-inside-four-pairs "[7,[6,[5,[4,[3,2]]]]]")))
    (is (= [12 "[13,2]"] (find-pair-of-regular-numbers-nested-inside-four-pairs "[7,[6,[5,[4,[13,2]]]]]")))
    (is (= [24 "[3,2]"] (find-pair-of-regular-numbers-nested-inside-four-pairs "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))
    (is (nil? (find-pair-of-regular-numbers-nested-inside-four-pairs "[[1,9],[8,5]]")))

    (is (= [13 56] (extract-numbers-from-pair "[7,[6,[5,[4,[13,56]]]]]" 12)))

    (is (= [10 "4"] (find-number-left-from "[7,[6,[5,[4,[13,2]]]]]" 12)))
    (is (nil? (find-number-left-from "[[[[[9,8],1],2],3],4]" 4)))

    (is (= [10 "1"] (find-number-right-from "[[[[[9,8],1],2],3],4]" 9)))
    (is (= [20 "6"] (find-number-right-from "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" 15)))
    (is (nil? (find-number-right-from "[7,[6,[5,[4,[3,2]]]]]" 16)))

    (is (= "[[6,[5,[7,0]]],3]" (explode-pair "[[6,[5,[4,[3,2]]]],1]" [10 "[3,2]"])))
    (is (= "[[[[0,9],2],3],4]" (explode-pair "[[[[[9,8],1],2],3],4]", [4 "[9,8]"])))
    (is (= "[7,[6,[5,[7,0]]]]" (explode-pair "[7,[6,[5,[4,[3,2]]]]]", [12 "[3,2]"])))
    (is (= "[[6,[5,[7,0]]],3]" (explode-pair "[[6,[5,[4,[3,2]]]],1]", [10 "[3,2]"])))
    (is (= "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" (explode-pair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", [10 "[7,3]"])))
    (is (= "[[3,[2,[8,0]]],[9,[5,[7,0]]]]" (explode-pair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", [24 "[3,2]"])))

    (is (= "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" (split "[[[[0,7],4],[15,[0,13]]],[1,1]]")))
    (is (= "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]" (split "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")))
    (is (= "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (split "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))

    (is (= "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (reduce-snailfish "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))

    (is (= "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" (add-snailfishes "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]")))

    (is (= "[[[[1,1],[2,2]],[3,3]],[4,4]]" (add-snailfish-list-from-file "example-18-1.txt")))
    (is (= "[[[[3,0],[5,3]],[4,4]],[5,5]]" (add-snailfish-list-from-file "example-18-2.txt")))
    (is (= "[[[[5,0],[7,4]],[5,5]],[6,6]]" (add-snailfish-list-from-file "example-18-3.txt")))
    (is (= "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" (add-snailfish-list-from-file "example-18-4.txt")))

    (is (= 29 (first (magnitude-of-pair "[9,1]" 0))))
    (is (= 143 (first (magnitude-of-pair "[[1,2],[[3,4],5]]" 0))))
    (is (= 1384 (first (magnitude-of-pair "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" 0))))
    (is (= 445 (first (magnitude-of-pair "[[[[1,1],[2,2]],[3,3]],[4,4]]" 0))))
    (is (= 791 (first (magnitude-of-pair "[[[[3,0],[5,3]],[4,4]],[5,5]]" 0))))
    (is (= 1137 (first (magnitude-of-pair "[[[[5,0],[7,4]],[5,5]],[6,6]]" 0))))
    (is (= 3488 (first (magnitude-of-pair "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" 0))))
    ))
