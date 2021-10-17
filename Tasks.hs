{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

module Tasks where

import Text.Printf
import Dataset
-- Postolache Alexanru-Gabriel 321CB
import Data.List
import Data.Maybe
import Data.Array
import Data.Tuple

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

-- Task 1
--next function will calculate (Q1+..+Q6)/4
questions_grade :: Row -> Float
questions_grade = (/ 4.00).(foldr (\x acc -> if x == "" || x == "." then (0.00 + acc)
    else ((read x :: Float) + acc)) 0.00)

-- next function will calculate the final grade of every student
final_grade_students :: Table -> [Float]
final_grade_students table = zipWith (\x y -> (read x :: Float) + (questions_grade y))
    (map last table) (map init table)

compute_exam_grades :: Table -> Table
compute_exam_grades table
    | head (head table) == "Nume" = ["Nume", "Punctaj Exam"] : 
        compute_exam_grades (tail table)
    | otherwise =  zipWith (\x y -> x : ((printf "%.2f" y) : []) ) 
        (map head table) (final_grade_students (map tail table))

-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num [] = 0
get_passed_students_num table = foldr checkPassed 0 (compute_exam_grades 
    (tail table))
    where
        checkPassed :: Row -> Int -> Int
        checkPassed row acc
            | (read (last row) :: Float) >= 2.5 = 1 + acc
            | otherwise = acc

-- function for finding the nr of students
nrTotalStudents :: Table -> Float
nrTotalStudents [] = 0.0
nrTotalStudents table = foldr (\x acc -> (1.00 + acc)) 0.00 (tail table)

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage [] = 0.00
get_passed_students_percentage table = (fromIntegral (get_passed_students_num
    table) :: Float) / (nrTotalStudents table)

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg [] = 0
get_exam_avg table = (foldr (\x acc -> (read (last x) :: Float) + acc) 0.0 
    (tail (compute_exam_grades table))) / (nrTotalStudents table)

-- function which keeps only the homeworks on the table
onlyHW :: Table -> Table
onlyHW = (map init).(map init).(map init).(map init).(map tail).(map tail)

addToSum :: String -> Float -> Float
addToSum x y
    | x == "" = y
    | x == "." = y
    | x == " " = y
    | otherwise = (read x :: Float) + y

-- returns the sum of row's elements
sumRow :: Row -> Float
sumRow [] = 0.0
sumRow row = foldr addToSum 0.0 row

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num [] = 0
get_passed_hw_num table = foldr checkScore 0 (onlyHW (tail table))
    where
        checkScore :: Row -> Int -> Int
        checkScore row acc
            | sumRow row >= 1.5 = 1 + acc
            | otherwise = acc    

-- Task 3
-- adds the results of one person at the interview's questions to a list where
-- the sum of every question will get stored
addToQs :: Row -> [Float] -> [Float]
addToQs [] [] = []
addToQs [] acc = acc
addToQs row [] = map (\x -> if x == "" then 0.0 else read x :: Float) row
addToQs row acc = zipWith addToSum row acc

-- calculates the average number from a list
calculateAverage :: Float -> [Float] -> Row
calculateAverage nr list = map (\x -> printf "%.2f" (x / nr)) list 

-- calculates the nr of participants at the exam
nrParticipants :: Table -> Float
nrParticipants [] = 0
nrParticipants table = foldr (\x acc -> if head x == "" then acc else 1+acc) 
    0.0 (map tail (tail table)) 

-- returns an array which contains the average of every question
get_list_avg_per_qs :: Table -> Row
get_list_avg_per_qs table = (calculateAverage (nrTotalStudents table) 
    (foldr (addToQs) [] (map init (map tail(tail table)))))

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = (tail (init (head table))) :
    (get_list_avg_per_qs table : [])

-- Task 4
-- returns from a table only the cells related to the interview questions
onlyQs :: Table -> Table
onlyQs table = (map tail (map init table))

-- it will increase in the acc the number of occurences of a certain found score 
count :: String -> Row -> Row
count el acc
    | el == "0" || el == "" = show ((read (head acc) :: Int) + 1) : (tail acc)
    | el == "2" = init acc ++ [show ((read (last acc) :: Int) + 1)]
    | el == "1" = head acc : (show ((read (head (tail acc)) :: Int) + 1) : 
        [(last acc)] )
    | otherwise = el:acc -- the name of the question Qi is added


-- function which returns an empty table
emptyTable :: Table -> Table
emptyTable table = foldr (\x acc -> []:acc) [] (map head table)

-- my idea is to go trought the collumns and for every column to store the result
-- in the accumulator
countQs :: Table -> Table
countQs table
    | table /= emptyTable table = (foldr (count) ["0", "0", "0"] (map head table)) 
        : (countQs (map tail table) )
    | otherwise = []

get_exam_summary :: Table -> Table
get_exam_summary table = ["Q","0","1","2"] : (countQs (onlyQs table))

-- Task 5
-- checks if a row has a bigger last Value than other one
isBigger :: Row -> Row -> Bool
isBigger row1 row2 
    | (read (last row1) :: Float) > (read (last row2) :: Float) = True
    | (read (last row1) :: Float) == (read (last row2) :: Float) && 
        (head row1 >= head row2) = True
    | otherwise = False

-- returns all the rows from a table who have the last value smaller than
-- another row received as parameter
smallerElements :: Row -> Table -> Table
smallerElements row table = filter (\x -> isBigger row x) table

-- returns all the rows from a table who have the last value bigger than
-- another row received as parameter
biggerElements :: Row -> Table -> Table
biggerElements row table = filter (\x -> (isBigger row x) == False) table 

-- sorts the table
sortTable :: Table -> Table
sortTable [] = []
sortTable (x:xs)  = (sortTable (smallerElements x xs)) ++ 
    (x : (sortTable (biggerElements x xs)))

get_ranking :: Table -> Table
get_ranking table = (head (compute_exam_grades  table)) : sortTable (tail 
    (compute_exam_grades table))

-- Task 6
-- calculates the interview scores for every student
getInterviewScore :: Table -> Row
getInterviewScore [] = []
getInterviewScore table = foldr (\x acc -> (printf "%.2f" 
    (questions_grade x)) : acc) [] table

-- build a new table with a few collons: names, interview scores
buildNewTable :: Table -> Table
buildNewTable [] = []
buildNewTable table = zipWith (\x y -> x:(y:[])) (map head table) 
    (getInterviewScore (map init (map tail table)))

-- transform a string "1.2" in "1.20"
to2Decimals :: Value -> Value
to2Decimals x = printf "%.2f" (read x :: Float)

-- adds a new collon with the grade from the written exam for every student
addWrittenGrade :: Table -> Table
addWrittenGrade table = zipWith (\x y -> x++[y]) (buildNewTable table) 
    (map to2Decimals (map last table))

-- calculates the diffrence between the 2 types of exams and adds it as a colon
-- in the table
getDiffField :: Table -> Table
getDiffField = map (\x -> x++[printf "%.2f" (abs((read (head(tail x)) :: Float) 
    - (read (head(tail (tail( x)))) :: Float)))])

-- return the wanted new table for Task6
getDiffrenceExamTable :: Table -> Table
getDiffrenceExamTable [] = []
getDiffrenceExamTable table = getDiffField (addWrittenGrade table)

get_exam_diff_table :: Table -> Table
get_exam_diff_table table = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"] 
    : (sortTable (getDiffrenceExamTable (tail table)))


{-
    TASK SET 2
-}

-- function to split a string
split :: Char -> String -> [String]
split d [] = [""]
split d (x:xs)
    | x == d = "" : (split d xs)
    | otherwise = (x : (head (split d xs))) : (tail (split d xs))

--citire csv
read_csv :: CSV -> Table
read_csv x = map (split ',') (split '\n' x)   

rowToString :: Row -> String
rowToString = foldr (\x acc -> "," ++ x ++ acc) ""

--scriere csv
write_csv :: Table -> CSV
write_csv = (tail ).(foldr (\x acc -> "\n" ++ (tail (rowToString x)) ++ acc) "") 

--Task 1
--find index of the column
indexColumn :: String -> Row -> Integer
indexColumn "" _ = 0
indexColumn name row
    | head row == name = 1
    | otherwise = 1 + (indexColumn name (tail row))

-- deletes all the columns untill we reach the wanted one
-- when the wanted columns is reached takes it's elements
deleteOtherColumns :: Table -> Integer -> Row
deleteOtherColumns table 0 = undefined
deleteOtherColumns table index
    | index == 1 = foldr (\x acc -> (head x) : acc) [] table--map head table
    | otherwise = deleteOtherColumns (map tail table) (index-1)

--takes a column name and a Table,returns the values from that column as a list
as_list :: String -> Table -> [String]
as_list column table =  deleteOtherColumns (tail table) (indexColumn column
    (head table))


-- Task 2
-- returns a cell's value with a certain index from a row
getCellOnColumn :: Integer -> Row -> String
getCellOnColumn index row
    | index == 1 = head row
    | otherwise = getCellOnColumn (index - 1) (tail row)

-- compare function used by sortBy to sort based on certains cells from rows
myCompare :: Integer -> Row -> Row -> Ordering
myCompare index r1 r2
    | (getCellOnColumn index r1) > (getCellOnColumn index r2) = GT
    | (getCellOnColumn index r1) == (getCellOnColumn index r2) &&
        (head r1) > (head r2) = GT
    | otherwise = LT

-- sort a table's rows except 1st using sortBy
tsort :: String -> Table -> Table
tsort column table = (head table) : sortBy (myCompare (indexColumn column
    (head table)) ) (tail table)


-- Task3
vmap :: (Value -> Value) -> Table -> Table
vmap f table = map (\x -> map f x) table
--- An example use of this would be:
--correct_exam_table = value_map (\x -> if x == "" then "0" else x) exam_grades

-- Task4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f row table = row : (map f (tail table))

--calculates the sum from collons 2 to end
sumColons :: Row -> Float
sumColons [] = 0
sumColons (x:xs)
    | x == "" = 0 + (sumColons xs)
    | otherwise = (read x :: Float) + (sumColons xs)

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row) : (printf "%.2f" (sumColons (tail (tail row)))
    : [])

-- Task 5
vunion :: Table -> Table -> Table
vunion t1 t2
    | head t1 == (head t2) = t1 ++ (tail t2)
    | otherwise = t1

-- Task6
-- ads collons to a table and makes sure the empty cells will be filled with empty
-- values
auxHunion :: Table -> Table -> Integer -> Table
auxHunion [] [] _ = []
auxHunion (x:xs) [] nr = (x ++ (fillRow nr [""])) : (auxHunion xs [] nr)
auxHunion [] (y:ys) nr = (fillRow nr [""] ++ y) : (auxHunion [] ys nr)
auxHunion (x:xs) (y:ys) nr = (x ++ y) : (auxHunion xs ys nr)

--fills a row with empty values
fillRow :: Integer -> Row -> Row
fillRow nr row
    | nr > 1 = fillRow (nr - 1) (row ++ [""]) 
    | otherwise = row

nrColumn :: Row -> Integer
nrColumn [] = 0
nrColumn row = 1 + (nrColumn (tail row)) 

nrRows :: Table -> Integer
nrRows [] = 0
nrRows (x:xs) = 1 + (nrRows xs)

hunion :: Table -> Table -> Table
hunion t1 t2
    | nrRows t1 > (nrRows t2) = auxHunion t1 t2 (nrColumn (head t2))
    | nrRows t1 < (nrRows t2) = auxHunion t1 t2 (nrColumn (head t1))
    | otherwise = auxHunion t1 t2 0

-- Task7
checkRow :: String -> Row -> Bool
checkRow value [] = False
checkRow value (x:xs)
    | value == x = True
    | otherwise = checkRow value xs

--verifica daca valoarea se gaseste intr-o tabela si returneaza randul cu valoarea
--daca nu se gaseste se returneaza rand gol
checkMatrix :: String -> Table -> Integer -> Row
checkMatrix value [] nrColumnsTable  = fillRow (nrColumnsTable - 1) [""]
checkMatrix value (x:xs) nrColumnsTable
    | checkRow value x == True = x
    | otherwise = checkMatrix value xs nrColumnsTable 

--construieste tabelul format fie din randuri ale lui t2 fie randuri goale daca
--nu se gaseste cheia
buildTable :: String -> Table -> Table -> Table
buildTable column t1 t2 =  foldr (\x acc -> (deleteElRow x (checkMatrix x 
    (tail t2) (nrColumn (head t2)))) : acc) [] (as_list column t1)

--deletes an element from a row
deleteElRow :: String -> Row -> Row
deleteElRow key [] = []
deleteElRow key (x:xs)
    | x == key = deleteElRow key xs
    | otherwise = x : (deleteElRow key xs) 

tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = (head t1 ++ (deleteElRow key (head t2))) : (hunion (tail t1)
    (buildTable key t1 t2))

-- Task8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f columns t1 t2 = columns :(foldr (\x acc -> foldr (\y acc-> (f x y)
    : acc ) acc (tail t2)) [] (tail t1))

-- Task9
--face o matrice din valorile coloanelor cautate adaugate ca rows
auxProjection :: [String] -> Table -> Table
auxProjection [] table = []
auxProjection (x:xs) table = (as_list x table) : (auxProjection xs table)

--transpose matrix
tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr m = (map head m) : (tr (map tail m))

--Extract only the specified colons
projection :: [String] -> Table -> Table
projection [] table = []
projection columns table = columns : (tr (auxProjection columns table))

{-
    TASK SET 3
-}

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
    
 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]
-- don't confuse first 'CSV' and second 'CSV': first refers to constructor name,
-- second refers to type CSV (defined in taskset 1); same with 'Table'.

-- ex 3.1
-- se foloseste la evaluarea query-urilor
instance Show QResult where
    show (CSV a_string) = show a_string
    show (Table table) = write_csv table
    show (List list) = show list

-- ex 3.2
class Eval a where
    eval :: a -> QResult
 
-- converteste un query in string si dupa intr-un tabel
to_table :: Query -> Table
to_table query = read_csv (show (eval query))

-- se aplica eval deoarece show este pt Qresult si eval returneaza Qresult
instance Eval Query where
    eval (FromCSV csv) = Table (read_csv csv)
    eval (ToCSV query) = CSV (show (eval query))
    eval (AsList colname query) = List (as_list colname (to_table query))
    eval (Sort colname query) = Table (tsort colname (to_table query))
    eval (ValueMap op query) = Table (vmap op (to_table query))
    eval (RowMap op colnames query) = Table (rmap op colnames (to_table query))
    eval (VUnion query1 query2) = Table (vunion (to_table query1) 
        (to_table query2))
    eval (HUnion query1 query2) = Table (hunion (to_table query1) 
        (to_table query2))
    eval (TableJoin colname query1 query2) = Table (tjoin colname 
        (to_table query1) (to_table query2))
    eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames 
        (to_table query1) (to_table query2))
    eval (Projection colnames query) = Table (projection colnames 
        (to_table query))
    -- 3.4
    eval (Filter condition query) = Table ((head (to_table query)) : (filter 
        (feval (head (to_table query)) condition) (tail (to_table query))))
    -- 3.5
    eval (Graph edgeop query) = Table ((["From", "To", "Value"]) : (build_graph 
        edgeop (tail (to_table query))))

-- face un Row in fct de valoarea lexicografica a elem
build_Row :: String -> String -> Value -> Row
build_Row s1 s2 val
    | s1 < s2 = s1 : (s2 : (val : []))
    | otherwise = s2 : (s1 : (val : []))

-- creaza un row pt tabelul de la 3.5
make_Row :: EdgeOp -> Row -> Row -> Row
make_Row f x y
    | isJust (f x y) == True = build_Row (head x) (head y) (fromJust (f x y))
    | otherwise = []

-- creaz tabelul pt legaturile nodului curent
aux :: EdgeOp -> Row -> Table -> Table
aux f x [] = []
aux f x (y:ys)
    | (make_Row f x y) /= [] = (make_Row f x y) : (aux f x ys) 
    | otherwise = aux f x ys

-- construieste un tabel dupa cum se vrea la ex 3.5
build_graph :: EdgeOp -> Table -> Table
build_graph f [] = []
build_graph f (x:xs) = (aux f x xs) ++ (build_graph f xs)

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- ex 3.3
-- cauta o valoare de tip Float intr-o lista
checkRowFloat :: Float -> [Float] -> Bool
checkRowFloat val [] = False
checkRowFloat val (x:xs)
    | val == x = True
    | otherwise = checkRowFloat val xs

-- lista de Float in lista de String
to_String :: [Float] -> Row
to_String [] = []
to_String (x:xs) = (show x) : (to_String xs)

--transforma din string in Float
read_to_Float :: String -> Float
read_to_Float el
    | el == "" = 0
    | otherwise = read el :: Float

instance FEval Float where
    feval columns (Eq colname value) = (\row -> ((read_to_Float (getCellOnColumn 
        (indexColumn colname columns) row)) == value))
    feval columns (Lt colname value) = (\row -> ((read_to_Float (getCellOnColumn 
        (indexColumn colname columns) row)) < value))
    feval columns (Gt colname value) = (\row -> ((read_to_Float (getCellOnColumn 
        (indexColumn colname columns) row)) > value))        
    feval columns (In colname list) =  (\row -> (checkRowFloat (read_to_Float 
        (getCellOnColumn (indexColumn colname columns) row)) list))           
    feval columns (FNot cond) = (\row -> if (feval columns cond row) == False 
        then True else False)
    feval columns (FieldEq colname1 colname2) = (\row -> ((read_to_Float 
        (getCellOnColumn (indexColumn colname1 columns) row)) == (read_to_Float 
            (getCellOnColumn (indexColumn colname2 columns) row))))


instance FEval String where
    feval columns (Eq colname value) = (\row -> (getCellOnColumn 
        (indexColumn colname columns) row == value))
    feval columns (Lt colname value) = (\row -> (getCellOnColumn 
        (indexColumn colname columns) row < value))
    feval columns (Gt colname value) = (\row -> (getCellOnColumn 
        (indexColumn colname columns) row > value))
    feval columns (In colname list) =  (\row -> (checkRow (getCellOnColumn 
        (indexColumn colname columns) row) list))                        
    feval columns (FNot cond) = (\row -> if (feval columns cond row) == False 
        then True else False)
    feval columns (FieldEq colname1 colname2) = (\row -> ((getCellOnColumn 
        (indexColumn colname1 columns) row) == (getCellOnColumn 
            (indexColumn colname2 columns) row)))


 -- ex 3.6

-- calculeaza distanta dintre 2 studenti
-- Row contine doar rezultatele la intrebari
distance :: Row -> Row -> Integer
distance [] [] = 0
distance (x:xs) (y:ys)
    | x == y = 1 + (distance xs ys)
    | otherwise = distance xs ys

-- conditia de legatura a graficului folosita la 3.6
edgeopSQ :: Row -> Row -> Maybe Value
edgeopSQ student1 student2
    | (head student1) == "" = Nothing
    | (head student2) == "" = Nothing
    | distance (tail student1) (tail student2) >= 5 = Just (show (distance 
        (tail student1) (tail student2)))
    | otherwise = Nothing

--transforma un QResult de tip Table in Query
tabel_to_query :: Table -> Query
tabel_to_query table = FromCSV (write_csv table)

-- compare function used by sortBy to sort based on certains cells from rows
myCompareValue :: Integer -> Row -> Row -> Ordering
myCompareValue index r1 r2
    | (read (getCellOnColumn index r1) :: Int) > (read (getCellOnColumn index 
        r2) :: Int) = GT
    | (getCellOnColumn index r1) == (getCellOnColumn index r2) &&
        (head r1) > (head r2) = GT
    | otherwise = LT

-- sort a table's rows except 1st using sortBy
tsort_Value :: String -> Table -> Table
tsort_Value column table = (head table) : sortBy (myCompareValue (indexColumn 
    column (head table)) ) (tail table)

-- sorteaza un QResult de tip Table si returneaza Tabelul
sort_QResult :: QResult -> Table
sort_QResult (Table table) = tsort_Value "Value" table
 
similarities_query :: Query
similarities_query = tabel_to_query (sort_QResult (eval (Graph edgeopSQ (FromCSV 
    (write_csv lecture_grades)))))

{-
    TASK SET 4
-}
--extracts the colName column from a table
extract_col :: String -> Table -> Row
extract_col colName table = as_list colName table

--return True if an element from T or Row has a match in the other
hasMatch :: Row -> String -> Bool
hasMatch [] s = False
hasMatch (x:xs) s
    | s == x = True
    | otherwise = hasMatch xs s

--filters the problematic values from a row
filterVal :: Row -> Row -> Row
filterVal t ref = (filter (hasMatch ref) t)

--calculates the distance between 2 strings using Wagner-Fischer Algorithm
distance_opt :: String -> String -> Int
distance_opt a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = listArray bounds [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), ((length a),(length b)))        

--find the minimum distance for an element of T to Ref to the correct string
-- returns the correct string
findDistanceMin :: Int -> Value -> Value -> Row -> String
findDistanceMin min valMin val [] = valMin
findDistanceMin min valMin val (x:xs)
    | (distance_opt val x) < min = findDistanceMin (distance_opt val x) x val xs
    | otherwise = findDistanceMin min valMin val xs

-- creates the column of the Typo with the correct values
findCorrectT :: Row -> Row -> Row
findCorrectT t ref = map (\x -> findDistanceMin 1000 "aux" x ref) t

--adds on element in a list on an index and eliminates the old value
addEl :: Integer -> String -> Row -> Row
addEl index s [] = []
addEl index s (x:xs)
    | index == 1 = s:xs
    | otherwise = x:(addEl (index - 1) s xs)

--adds a colon on a position in a table
addColon :: Integer -> Row -> Table -> Table
addColon index row table = zipWith (addEl index) row table

--builds the body of the Typo table
buildTypo :: String -> Row -> Table -> Table
buildTypo name t table = ((head table) : (addColon (indexColumn name (head table))
    t (tail table)))

--auxiliar function
correct_table2 :: String -> Table -> Table -> Table
correct_table2 name t ref = buildTypo name (findCorrectT (extract_col name t) 
    (extract_col name ref)) t

-- this will be tested using: correct_table "Nume" email_map_csv hw_grades_csv
correct_table :: String -> CSV -> CSV -> CSV
correct_table nameCol t ref = write_csv (correct_table2 nameCol (read_csv t) 
    (read_csv ref))

--calculates the sum of a hw_grades table
sum_hw_grade :: Row -> String
sum_hw_grade [] = ""
sum_hw_grade row = printf "%.2f" (sumRow row)

nrColumnFloat :: Row -> Float
nrColumnFloat [] = 0.0
nrColumnFloat row = 1.0 + (nrColumnFloat (tail row))

--calculates the sum of a lecture_grades table
sum_lecture_grades :: Row -> String
sum_lecture_grades [] = ""
sum_lecture_grades row = printf "%.2f" ((2 * (sumRow row)) / (nrColumnFloat row))

--calculates the sum of questions
questions_grade_cal :: Row -> Float
questions_grade_cal [] = 0.0
questions_grade_cal (x:xs)
    | x == "" || x == " " = 0.0 + (questions_grade_cal xs)
    | otherwise = (read x :: Float) + (questions_grade_cal xs)

sum_exam_grade :: Row -> String
sum_exam_grade [] = ""
sum_exam_grade row = printf "%.2f" (questions_grade (init row) + (readAux 
    (last row)))

--read for special cases like " " read
readAux :: String -> Float
readAux a
    | a == "" || a == " " = 0.0
    |otherwise = (read a :: Float) 

total :: String -> String -> String -> String
total h2_grade lecture_grade exam_grade
    | (readAux exam_grade) < 2.5 = "4.00"
    | (readAux h2_grade) + (readAux lecture_grade) < 2.5 = "4.00"
    | otherwise = printf "%.2f" (minimum [(readAux h2_grade) + (readAux 
        lecture_grade), 5] + (readAux exam_grade))

make_Row2 :: String -> String -> String ->String -> Row
make_Row2 name sum_hw sum_lecture sum_exam = (name:(sum_hw:(sum_lecture:(sum_exam:
    ((total sum_hw sum_lecture sum_exam):[])))))

--finds a row after the first elem and returns it
findRow :: String -> Table -> Row
findRow name [] = [" "]
findRow name (x:xs)
    | name == (head x) = x
    |otherwise = findRow name xs

--creates a table according to the last excersise which depends on the table's 
--know structure
createTable :: Table -> Table -> Table -> Table -> Table
createTable [] _ _ _ = []
createTable (x:xs) hw exam lect = ((make_Row2 (head x) (sum_hw_grade (tail 
    (findRow (head x) hw))) (sum_lecture_grades (tail (findRow (last x) lect)))
        (sum_exam_grade (tail (findRow (head x) exam)))):(createTable xs hw exam lect))

--auxiliar function
gradesAux :: Table -> Table -> Table -> Table -> Table
gradesAux t1 t2 t3 t4 = (tsort "Nume" (["Nume", "Punctaj Teme", "Punctaj Curs",
    "Punctaj Exam", "Punctaj Total"] : (createTable (tail t1) (tail t2) (tail t3)
        (tail t4))))

-- tested with: grades email_map_csv hw_grades_csv exam_grades_csv lecture_grades_csv
grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades t1 t2 t3 t4 = write_csv (gradesAux (read_csv (correct_table "Nume" t1 t2)) 
    (read_csv t2) (read_csv t3) (read_csv t4))