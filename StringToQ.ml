let string_to_q str =
	try
		let ind = String.index str '.' in
			let denompow = (String.length str) - ind - 1 in
				let numerator = String.concat "" [(String.sub str 0 ind) ; (String.sub str (ind + 1) ((String.length str) - ind -1))] in
					(int_of_string numerator, int_of_float ((float_of_int 10) ** (float_of_int denompow)))
	with Not_found ->
		(int_of_string str, 1)