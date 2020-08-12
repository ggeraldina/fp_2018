(*
use("D:/fp_2018/lab/labs.sml");
 
Лабораторная работа №1:

1. Определите функцию (min3) от трех аргументов целого типа, 
возвращающую наименьший из аргументов. Использовать 
вызов функции от двух аргументов (написать свою или 
использовать стандартную). 
*)

fun min3 x y z = Int.min((Int.min (x, y)), z);

min3 56 78 3;
min3 6 1 3;
min3 ~5 1 3;

(* 
2. Используя конструкцию let, упростите запись выражения 
(5 * 4 + (5 * 4 - 72)) div 5 + (5 * 4 - 72)*(5 * 4 - 72) - 5 * 4 
*)

let 
    val a = 5 * 4
    val b = a - 72 
in
    (a + b) div 5 + b * b - a
end;

(*
3. Используя конструкцию local, определите функцию 
«учетверения» строки (quadr), использующую локальную 
функцию «удвоения» строки (dupl). Например:
dupl "go " = "go go "
quadr "go " = "go go go go "
*)

local
    fun dupl str2:string = str2 ^ str2
in
    fun quadr str4:string = (dupl str4) ^ (dupl str4)
end;

quadr "go ";

(*
4. Определите функцию ComplexModule вычисления модуля 
комплексного числа. Функция получает в качестве параметра 
запись (record) с полями re и im типа real, возвращает real. 
Использовать функцию sqrt модуля Math.
*)

fun ComplexModule {re:real, im:real}:real =
    Math.sqrt(re * re + im * im);

ComplexModule {re = 56.7, im = 9.4};
ComplexModule {im = ~1.9, re = 0.0};

(*
5. Определите функцию «переворота» строки (trans). 
Например, trans "1234" = "4321". 
Использовать рекурсию. 
*)

fun trans (str):string = 
let
    fun transT (tail, ""):string = tail
      | transT (tail, str):string = 
    transT ((substring (str, 0, 1) ^ tail), 
    substring (str, 1, size (str) - 1))
in
    transT ("", str)
end;

trans "1234";
trans "abcdef";
trans "1";
trans "";


(*
Лабораторная работа №2:

1. Определите функцию:
a) power(x,n)=xn;
*)


fun power (x, 0) = 1.0
  | power (x, n)= 
    if n > 0 
    then power (x, n-1) * x
    else power (x, n+1) / x;

fun powerInt (x, 0) = 1
  | powerInt (x, n)= 
    if n > 0 
    then powerInt (x, n-1) * x
    else 0;

power (6.0, 0);
power (2.0, ~2);
powerInt (2, 5);


(*
b) seq_elem(n) - получение n-го элемента 
последовательности {5, 8, 11, 14, 17, 20, …};
*)

fun seq_elem(1) = 5	
  | seq_elem(n) = 
    if n > 0 
    then seq_elem(n-1) + 3
    else seq_elem(n+1) - 3;

seq_elem(5);
seq_elem(1);
seq_elem(0);


(*
2. Определите функцию (n может принимать значения ?0):
a) mult_all_even(n) - произведение всех четных чисел 
в промежутке [2,n];
*)

fun mult_all_even(0) = 0
  | mult_all_even(1) = 2
  | mult_all_even(2) = 2
  | mult_all_even(n) =
    if n < 0 
    then 0 
    else
         if n mod 2 = 1 
         then mult_all_even(n-1)
         else n * mult_all_even(n-2);

mult_all_even(9);
mult_all_even(2);
mult_all_even(~9);

(*
b) mult_all_odd(n) - произведение всех нечетных чисел 
в промежутке [1,n].
*)

fun mult_all_odd(0) = 0
  | mult_all_odd(1) = 1
  | mult_all_odd(2) = 1
  | mult_all_odd(n) =
    if n < 0 
    then 0 
    else
        if n mod 2 <> 1 
        then mult_all_odd (n-1)
        else n * mult_all_odd(n-2);

mult_all_odd(7);
mult_all_odd(1);
mult_all_odd(~5);

(*
3. Определите функцию
(x - список целых чисел, может быть пустым):
a) min(x) - минимальный элемент списка (min(nil)=0);
*)

fun min(nil) = 0
  | min(h::t) = 
    if t = nil 
    then h 
    else 
        if h < min(t) 
        then h 
        else min(t);

min [];
min [1, 2, 3, ~4, 5];

(*
b) mult(x) - произведение всех 
элементов списка (mult(nil)=0).
*)

fun mult(nil) = 0
  | mult(h::t) =
    if t = nil 
    then h 
    else h * mult(t);

mult [];
mult [2, 4, ~6, 1, 8, 3];

(*
Определите функцию sort(x) сортировки списка 
по возрастанию методом пузырька.
*)

local
    fun sort1(nil) = nil
      | sort1([x]) = [x]
      | sort1(h1::h2::t) =    
        if h1 > h2 
        then h2::sort1(h1::t)
        else h1::sort1(h2::t)
in
    fun sort(nil) = nil
      | sort(head::tail) = sort1(head::sort(tail))
end;

sort [2, 4, ~6, 1, 8, 3];
sort [];

(*
Лабораторная работа №3:

1. Определите функцию
a) вычисления количества единиц в списке:
count_1s([4,3,1,6,1,1,2,1]) = 4
*)

fun count_1s(nil) = 0
  | count_1s(h::t) = 
    if h = 1 
    then 1 + count_1s(t) 
    else count_1s(t);

count_1s([4,3,1,6,1,1,2,1]);
count_1s([4,3,6,2]);

(*
b) определения принадлежности элемента списку:
member(5, [4, 2, 5]) = true, 
member(3, [4, 2, 5]) = false
*)

fun member(x, nil) = false
  | member(x, h::t) = if x = h then true else member(x, t);

member(5, [4, 2, 5]);
member(3, [4, 2, 5]);

(*
2. Определите функции
a) выделения первых n элементов списка:
firstN(3, [4,3,1,6,1,1,2,1]) = [4,3,1]
*)

fun firstN(_, nil) = nil
  | firstN(0, _) = nil
  | firstN(n, h::t) = h::firstN(n-1, t);

firstN(3, [4,3,1,6,1,1,2,1]);
firstN(6, []);
firstN(0, [4,3,1,6,1,1,2,1]);

(*
b) выделения «хвоста» списка, начинающегося с (n+1)-го элемента:
fromN(3, [4,3,1,6,1,1,2,1]) = [6,1,1,2,1]
*)

fun fromN(_, nil) = nil
  | fromN(0, list) = list
  | fromN(n, h::t) = fromN(n-1, t);

fromN(3, [4,3,1,6,1,1,2,1]);
fromN(7, []);
fromN(0, [4,3,1,6,1,1,2,1]);

(*
3. Определите функцию
a) вставки элемента в упорядоченный список:
insert(5, [2,3,6,9]) = [2,3,5,6,9]
*)

fun insert(x, nil) = [x]
  | insert(x, h::t) = 	
    if x <= h 	
    then x::h::t 
    else h::insert(x, t);

insert(5, [2,3,6,9]);
insert(10, [2,3,6,9]);
insert(~8, [2,3,6,9]);

(*
b) построения списка чисел в заданном интервале:
upto(6, 9) = [6, 7, 8, 9]
*)

fun upto(x, y) = 
if x = y 
then [x]
else 
    if x < y
    then x::upto(x+1, y)
    else upto(y, x);

upto(6, 9);
upto(~1, 9);
upto(6, 1);

(*
Переделайте функцию firstword таким образом, чтобы она правильно 
обрабатывала строки с пробелами в начале:
firstword'(" My name is Boris") = "My"
(подсказка: определите и используйте функцию удаления пробелов в начале списка)
*)

local
    fun deletespace(nil) = nil
      | deletespace(#" "::t) = deletespace(t)
      | deletespace(list) = list
    fun fw(nil) = nil
      | fw(#" "::t) = nil
      | fw(h::t) = h :: fw(t)
in
    val firstword' = implode o fw o deletespace o explode
end;

firstword'(" My name is Boris");
firstword'("      My     name is Boris");
firstword'("");
firstword'("  ");

(*
4. Определите функцию преобразования строки (с пробелами) в список слов:
scanlex("My name is Boris") = ["My", "name", "is", "Boris"]
(подсказка: кроме firstword', определите и используйте также функцию 
butfirstword' выделения «хвоста» строки, начинающегося с первого пробела)
*)

local
    fun deletespace(nil) = nil
      | deletespace(#" "::t) = deletespace(t)
      | deletespace(list) = list
    fun fw(nil) = nil
      | fw(#" "::t) = nil
      | fw(h::t) = h :: fw(t) 
    fun butfw(nil) = nil
      | butfw(#" "::x::t) = 
    if x = #" " 
    then butfw(x::t) 
    else x::t
      | butfw(h::t) = butfw(t)
    val firstword' = implode o fw o deletespace o explode
    val butfirstword' = implode o butfw o deletespace o explode
in
    fun scanlex("") = nil
      | scanlex(list) = 
    firstword'(list)::scanlex(butfirstword'(list))
end;

scanlex("My name is Boris");
scanlex("        My    name   is   Boris  ");
scanlex(" ");
scanlex("");

(*
Лабораторная работа №4:

1. Определите функцию при помощи функции map:
a) fttl(["strange", "show", "think"])=["range", "ow", "ink"]
*)

local
    fun map f nil = nil
      | map f (h::t) = (f h)::(map f t);
    fun remove2symb(nil) = nil
      | remove2symb(h::t) = if t = nil then t else tl(t)
    val remove2symbol = implode o remove2symb o explode
in
    fun fttl(list) = map remove2symbol (list)
end;

fttl(["strange", "show", "think"]);
fttl(["s", "sh", ""]);

(*
b) fsml(["war", "la", "tea"])= ["swarm", "slam", "steam"]
*)

(*  1 version *)
local
    fun map f nil = nil
      | map f (h::t) = (f h)::(map f t)
    fun add2symbol(str)= "s" ^ str ^ "m"     
in
    fun fsml (list) = map add2symbol (list)
end;

fsml(["war", "la", "tea"]);
fsml(["r", "la", ""]);

(*  2 version *)
local
    fun map f nil = nil
      | map f (h::t) = (f h)::(map f t)    
in
    val fsml = map (fn str => "s" ^ str ^ "m")
end;

fsml(["war", "la", "tea"]);
fsml(["r", "la", ""]);

(*
2. Определите функции при помощи функции reduce:
a) count [3,2,5,1] = 4
*)

local
    fun reduce f b nil = b
      | reduce f b (h::t) = f(h, reduce f b t)
in
    val count = reduce (fn(_:int, a:int) => a + 1) 0
end;

count [3,2,5,1];
count [3,2,5,1,3,2,5,3,2,5];
count [];

(*
a) duplist [4,2,5,1] = [4,4,2,2,5,5,1,1]
*)

local
    fun reduce f b nil = b
      | reduce f b (h::t) = f(h, reduce f b t)
in
    val duplist = reduce (fn(a:int,b:int list)=>a::a::b) nil
end;

duplist [4,2,5,1];
duplist [];

(*
3. Определите функцию:
a) определения максимального из всех значений, хранящихся на узлах данного 
дерева (один входной параметр - дерево, возвращает максимальное значение);
*)

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;
fun maxval(Empty) = ~0.1/0.0
  | maxval(Node(lft, v, rht)) =
Real.max(v,  Real.max(maxval(lft), maxval(rht)));

maxval(Node(Node(Empty, ~5.0, Node(Node(Empty,~1.0, Empty),
 ~8.0, Empty)), ~4.0, Empty));

maxval(Node(Empty, 5.9, Empty));

maxval(Empty);

(*
b) подсчета количества узлов, содержащих данное значение (два входных 
параметра - дерево и значение, возвращает число узлов).
*)

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;
fun countx(Empty, _) = 0
  | countx(Node(lft, v, rht), x) = 
    if v = x 
    then 1 + countx(lft, x) + countx(rht, x) 
    else countx(lft, x) + countx(rht, x);

countx((Node(Node(Node(Node(Empty, 5, Empty), 4, Empty), 5, Empty), 4, 
Node(Node(Node(Node(Empty, 8, Empty), 4, Empty), 5, Empty), ~2, Empty))), 4);

countx((Node(Node(Node(Node(Empty, 5, Empty), 4, Empty), 5, Empty), 4, 
Node(Node(Node(Node(Empty, 8, Empty), 4, Empty), 5, Empty), ~2, Empty))), 8);

countx((Node(Node(Node(Node(Empty, 5, Empty), 4, Empty), 5, Empty), 4, 
Node(Node(Node(Node(Empty, 8, Empty), 4, Empty), 5, Empty), ~2, Empty))), 0);


(*
4. Определите функцию:
a) удаления пробелов из текстового файла (два входных параметра - 
имена исходного и результирующего файлов);
*)

local
    fun fileToString fileName =
    let
        val stream = TextIO.openIn(fileName)
        val str = TextIO.inputAll(stream)
        val _ = TextIO.closeIn(stream)
    in
        str
    end;
    fun stringToFile(str, fileName) =
    let
        val stream = TextIO.openOut(fileName)
        val _ = TextIO.output(stream, str)
        val _ = TextIO.closeOut(stream)
    in
        str
    end;
    fun delspace(nil) = nil
      | delspace(#" "::t) = delspace(t)
      | delspace(h::t) = h::delspace(t)
    val str = implode o delspace o explode o fileToString
in
    fun deletespace (fileNameIn, fileNameOut) =    
    stringToFile(str (fileNameIn), fileNameOut)
end;

deletespace("/media/geraldina/A8FD-DB6D/fp_2018/lab/smlExampleIn_lab4_4a.txt", 
"/media/geraldina/A8FD-DB6D/fp_2018/lab/smlExampleOut_lab4_4a.txt");

deletespace("D:/fp_2018/lab/smlExampleIn_lab4_4a.txt", 
"D:/fp_2018/lab/smlExampleOut_lab4_4a.txt");

(*
b) получения пустого текстового файла (составленного из пробелов) 
заданной длины (два входных параметра - имя результирующего файла и его длина).
*)

local
    fun fileToString fileName =
    let
        val stream = TextIO.openIn(fileName)
        val str = TextIO.inputAll(stream)
        val _ = TextIO.closeIn(stream)
    in
        str
    end;
    fun stringToFile(str, fileName) =
    let
        val stream = TextIO.openOut(fileName)
        val _ = TextIO.output(stream, str)
        val _ = TextIO.closeOut(stream)
    in
        str
    end;
    fun space(0) = ""
      | space(n) = " "^space(n-1)
in
    fun emptyfile (fileName, lenght) =    
    stringToFile(space lenght, fileName)
end;

emptyfile("/media/geraldina/A8FD-DB6D/fp_2018/lab/smlExample_lab4_4b.txt", 3);

emptyfile("D:/fp_2018/lab/smlExample_lab4_4b.txt", 3);


(*
Лабораторная работа №5:
(подсказка: определите и используйте вспомогательные функции) 

1. Множества задаются неупорядоченными списками своих 
элементов (без повторений). 
Определите функции:
a) объединения множеств:
setunion([9,2,7],[3,2])=[9,2,7,3]
*)

local
   fun member x nil = false
     | member x (h::t) = x = h orelse member x t
in
   fun setunion (list1: int list, nil) = list1
     | setunion (list1, h::t) = 
   if member h list1 
   then setunion (list1, t) 
   else setunion (list1 @ [h], t)
end;

setunion([9,2,7],[3,3,2]);
setunion([],[]);
setunion([],[3,2]);
setunion([9,2,7],[]);
setunion([9,7],[3,2]);

(*
b) пересечения множеств:
setintersection([9,2,7],[3,2])=[2]
*)

local
    fun member x nil = false
      | member x (h::t) = x = h orelse member x t
    fun intersection(_, nil, istresult) = istresult
      | intersection(list1, h::t, listresult) = 
    if member h list1 
    then intersection(list1, t, listresult @ [h])
    else intersection(list1, t, listresult)
in
    fun setintersection(nil, nil) = nil
      | setintersection(list1: int list, list2: int list) = 
    intersection(list1, list2, nil)
end; 

setintersection([9,2,7],[3,2]);
setintersection([],[]);
setintersection([],[3,2]);
setintersection([9,2,7],[]);
setintersection([9,7],[3,2]);

(*
3. Определите функцию, меняющую местами заданные элементы списка:
exchange([3,2,5,1],1,3) = [5,2,3,1]
*)

local
    fun tmp2(nil, second) = 0
      | tmp2(h::t, second) =  
    if second = 1 
    then h 
    else tmp2(t, second - 1);
    fun change2(nil, second, tmp, listresult) = nil
      | change2(h::t, second, tmp, listresult) =
    if second = 1
    then listresult @ [tmp] @ t    
    else change2(t, second - 1, tmp, listresult @ [h]);
    fun change(nil, first, second, tmp, listresult) = nil
      | change(h::t, first, second, tmp, listresult) = 
    if first = 1
    then change2(t, second - 1, h, listresult @ [tmp2(t, second - 1)])
    else change(t, first - 1, second - 1, tmp, listresult @ [h]) 
in
    fun exchange(nil, first, second) = nil
      | exchange(list, first, second) = 
    if first > length(list) orelse second > length(list) 
       orelse first <= 0 orelse second <= 0
    then nil
    else
        if first = second 
        then list
        else 
            if second < first 
            then exchange(list, second, first)
            else change(list, first, second, 0, nil)
end;

exchange([3,2,5,1],1,3);
exchange([3,2,5,1],4,2);
exchange([3,2,5,1],1,9);

(*
4. Определите функцию, по данному списку элементов строящую список пар вида 
(элемент, число вхождений в список):
stat(["a","b","b","c","b","a"]) = [("a",2),("b",3),("c",1)]
*)

local
    fun add(x, nil, head)= head @ [(x, 1)]
      | add(x, (smb, count)::t, head) = 
    if smb = x 
    then head @[(smb, count + 1)] @ t
    else add(x, t, head @  [(smb, count)]);
    fun addlist(nil, result) = result
      | addlist(h::t, result) =  addlist(t, add(h, result, nil))
in
    fun stat(nil) = nil
      | stat(list) = addlist(list, nil)
end;

stat(["a","b","b","c","b","a"]);
stat(["a","6","b","c","1","a"]);
stat([1,3,5,1,5,7]);
stat(["a","b","a"]);
stat([""]);
stat [];

