import Data.List(group)
--geriye fonksiyon listesi dönen bir metod
makeFunctionList ::[String] -> [Integer->Integer]
--makeFunctionList String_dizi
makeFunctionList [] = []
makeFunctionList ("+":x:xs) = (+(read x::Integer)):makeFunctionList xs
makeFunctionList ("-":x:xs) = (subtract (read x::Integer)):makeFunctionList xs
makeFunctionList ("/":x:xs) = (`div`(read x::Integer)):makeFunctionList xs
makeFunctionList ("*":x:xs) = (* (read x::Integer)):makeFunctionList xs

--foldl kullanılarak fonksiyon listesi üzerine istenen baslangic degeri uygulanıyor
recurrence::String->Integer->Integer->Integer
--recurrence ifade baslangic_degeri terim_sayisi
recurrence x first 0 = first
recurrence x first count =  foldl (\acc f -> f acc) current funcitonList
		where
			funcitonList = makeFunctionList$words$x
			current = recurrence x first (count-1)