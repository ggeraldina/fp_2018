(*
Индивидуальная лабораторная работа

11. Алгоритм вычисления всех простых чисел в заданном отрезке. 
Реализовать два способа: решето Эратосфена и решето Сундарама.
*)

(* 
решето Эратосфена
Sieve of Eratosthenes

Для нахождения всех простых чисел не больше заданного числа n, 
следуя методу Эратосфена, нужно выполнить следующие шаги:

1. Выписать подряд все целые числа от двух до n (2, 3, 4, …, n).
2. Пусть переменная p изначально равна двум — первому простому числу.
3. Зачеркнуть в списке числа от 2p до n считая шагами по p 
(это будут числа кратные p: 2p, 3p, 4p, …).
4. Найти первое незачёркнутое число в списке, большее чем p,
и присвоить значению переменной p это число.
5. Повторять шаги 3 и 4, пока возможно.
Теперь все незачёркнутые числа в списке 
— это все простые числа от 2 до n.
*)
fun primeE (n1, n2)=
let	
	(* 
	создать список с 2 до n2 + 1 из значений true 
	*)
	fun uptoBool(x, y) = 
		if x = y then [true] 
		else 
			if x <= y then true::uptoBool(x+1, y)
			else uptoBool(y, x)
	val list = uptoBool(2, n2)
	(* 
	отметить кратные числу number
	*)
	fun multipN(head, nil, _, _, _) = head
	  | multipN(head, h::t, number, count, flagFirst) = 
	  if count = number andalso flagFirst <> true 
	  then multipN(head @ [false], t, number, 1, flagFirst)
	  else if count = number andalso flagFirst <> false 
	       then multipN(head @ [h], t, number, 1, false)
		   else multipN(head @ [h], t, number, count + 1, flagFirst)
	(* 
	отметить каждое кратное числу от 2 до n2 + 1 
	*)
	fun sieveE(list, count) = 
		if count * count > n2 then list
		else sieveE(multipN(nil, list, count, 2, true), count + 1)
	val list = sieveE(list, 2);
	(*
	создать список с результатом 
	*)
	fun prime([], result, _) = result
	  | prime(h::t, result, count) =   
	  if count >= n1 andalso h <> false then prime(t, result @ [count], count + 1)
	  else prime(t, result, count + 1)
in 
	prime(list, nil, 2)
end;

primeE(8, 20);
primeE(3, 30);
primeE(36, 30);
primeE(1, 10);
primeE(2, 19);
primeE(1, 18);
primeE(356, 387);

(* 
решето Сундарама
Sieve of Sundaram

Алгоритм предусматривает исключение из ряда натуральных чисел от 1 до N 
всех чисел вида: i + j + 2ij, где индексы i <= j пробегают все натуральные 
значения, для которых i + j + 2ij <= N, а именно значения 
i = 1, 2, .. <= (sqrt(2N + 1) - 1) / 2 
и j = i, i + 1, .. <= (N - i) / (2i + 1). 
Затем каждое из оставшихся чисел умножается на 2 и увеличивается на 1. 
Полученная в результате последовательность представляет собой все 
простые числа в отрезке [3, 2N + 1]
*)

fun primeS(n1, n2) =
let    
	(* 
	вставить элемент, если его в списке еще нет 
	*)
	fun insertNoRepeat(x, nil) = [x]
  	  | insertNoRepeat(x, h::t) = 
	  	if x = h then h::t
		else if x <= h then x::h::t 
			 else h::insertNoRepeat(x, t)	
	(* 
	верхняя граница итератора i 
	*)		 
	val limitI = Real.floor(Math.sqrt(2.0 * Real.fromInt(n2) + 1.0) / 2.0)
	(*
	верхняя граница итератора j
	*)
	fun limitJ (i) = (n2 - i) div (2 * i + 1)	
	(* 
	создать вспомогательный список из d чисел таких, 
	что 2 * d + 1 - это составные числа 
	*)
	fun createHelpList(list, i, j) = 
	if i > limitI then list
	else if j > limitJ(i) then createHelpList(list, i + 1, 1)
		 else 
		 let
		 	 val x = i + j + 2 * i * j 
             val number = 2 * x  + 1
		 in
			 if number >= n1 then createHelpList(insertNoRepeat(x, list), i, j + 1)
			 else createHelpList(list, i, j + 1)
	     end
	(* 
	вспомогательный список 
	*)	 
	val helpList = createHelpList(nil, 1, 1)
	(* 
	создать список простых чисел, начиная с числа 3 или большего 
	*)
	fun prime(nil, result, count) =  		
		let
			val number = 2 * count + 1
		in
			if number > n2 then result 			
			else if number < n1 then prime([], result, count + 1)
				 else prime([], result @ [number], count + 1)
		end	
	  | prime(h::t, result, count) = 	  	
		let
			val number = 2 * count + 1
		in
			if number > n2 then result			
			else if number < n1 then prime(h::t, result, count + 1)
				 else if h = count then prime(t, result, count + 1)
				      else prime(h::t, result @ [2 * count + 1], count + 1)
		end	
in
	(* 
	добавить в список число 2, если это необходимо 
	*)
	if n1 <= 2 then 2::prime(helpList, nil, 1)
	else prime(helpList, nil, 1)
end;

primeS(8, 20);
primeS(3, 30);
primeS(36, 30);
primeS(1, 10);
primeS(2, 19);
primeS(1, 18);
primeS(356, 387);












































