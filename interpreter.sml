fun interpreter(inFile : string, outFile : string) =
let
	val inStream = TextIO.openIn inFile;
	val outStream = TextIO.openOut outFile;
	val readLine = TextIO.inputLine inStream;
	datatype values = INT of int | STR of string | BOOL of string | NAME of string | ERROR of string | UNIT of string | FUN of string 
	datatype commands = PUSH of values | POP | ADD | QUIT | MUL | NEG | DIV | REM | SUB | SWAP | OR | IF | AND | BIND | LET | END | NOT | Equal | LessThan | FUNEND | FUNDEF of values| FUNNAME of string | FUNARG of string | CALL | RETURN


	fun readfromfile(readLine : string option) =
		case readLine of
				NONE => []
			| SOME(c) => c::readfromfile(TextIO.inputLine inStream)

	fun checkifint(i : string) =
		case Int.fromString i of
		 	NONE => false
		 	| _ => true;
  
	
	fun commandlist(x: string) = 
		case String.tokens Char.isSpace x of
			["add"] => ADD 
			|["pop"] => POP 
			|["mul"] => MUL 
			|["div"] => DIV
			|["rem"] => REM
			|["sub"] => SUB
			|["neg"] => NEG
			|["swap"] => SWAP
			|["or"] => OR
			|["and"] => AND
			|["bind"] => BIND
			|["if"] => IF
			|["let"] => LET
			|["equal"] => Equal
			|["funEnd"] => FUNEND
			|["lessThan"] => LessThan
			|["end"] => END
			|["not"] => NOT
			|[":true:"] => (PUSH(BOOL(":true:")))
			|[":false:"] => (PUSH(BOOL(":false:")))
			|[":error:"] => (PUSH(ERROR(":error:")))
			|["quit"] => QUIT 
			|["call"] => CALL
			|["return"] => RETURN
			|[_] => (PUSH(ERROR(":error:")))


	fun	doesfunctorfpartwo([],c) = false
		|doesfunctorfpartwo((FUNNAME(x),FUNARG(y),z,sc)::funclist,c)= if x=c then true else doesfunctorfpartwo(funclist,c)	

	fun doesfunctorf(funclist,0,c) = false
		|doesfunctorf(altfs::funclist,i,c) = if doesfunctorfpartwo(altfs,c) then true else doesfunctorf(funclist,i-1,c)

	fun meattwo((FUNNAME(x),FUNARG(y),z,sc)::altstack,t) = if x=t then z else meattwo(altstack,t) 

	fun meat(altstack::funcstack,t) =  if doesfunctorfpartwo(altstack,t) then meattwo(altstack,t) else meat(funcstack,t) 

	fun turkeytwo((FUNNAME(x),FUNARG(y),z,sc)::altstack,t) = if x=t then sc else turkeytwo(altstack,t)

	fun turkey(altstack::funstack,t) = if doesfunctorfpartwo(altstack,t) then turkeytwo(altstack,t) else turkey(funstack,t)

	fun getardbindingtwo((FUNNAME(x),FUNARG(y),z,sc)::altstack,t) = if x=t then y else getardbindingtwo(altstack,t)

	fun getardbinding(altstack::funstack,t) = if doesfunctorfpartwo(altstack,t) then getardbindingtwo(altstack,t) else getardbinding(funstack,t)

	fun getarg(i:string) = 
		case String.tokens Char.isSpace i of
			[x,y] => y

	fun getname(i:string) = 
		case String.tokens Char.isSpace i of
			[x,y] => x


	fun getpushstring([]) = []
		|getpushstring(x::xs) = if size(x)>3 andalso substring(x,0,4)="push" andalso Char.isAlpha(String.sub(substring(x,5,size(x)-5),0))=false andalso  substring(substring(x,5,size(x)-5),0,1)<>"\"" andalso checkifint(substring(x,5,size(x)-6))=false  then (PUSH(ERROR(":error:"))::getpushstring(xs)) else if size(x)>2 andalso substring(x,0,3)="fun" andalso substring(x,3,3)<>"End"  then (FUNDEF(FUN(substring(x,4,size(x)-5)))::getpushstring(xs))   else if size(x)>3 andalso substring(x,0,4)="push" andalso substring(substring(x,5,size(x)-5),0,1)="\"" then (PUSH(STR(substring(x,6,size(x)-8)))::getpushstring(xs)) else if size(x)>3 andalso substring(x,0,4)="push" andalso Char.isAlpha(String.sub(substring(x,5,size(x)-5),0)) then (PUSH(NAME(substring(x,5,size(x)-6)))::getpushstring(xs)) else if size(x)>3 andalso substring(x,0,4)="push" andalso checkifint(substring(x,5,size(x)-5)) andalso Char.notContains (substring(x,5,size(x)-5)) #"." then (PUSH(INT(valOf(Int.fromString (substring(x,5,size(x)-6))))):: getpushstring(xs)) else if size(x)>3 andalso substring(x,0,4)="push" andalso checkifint(substring(x,5,size(x)-6)) andalso Char.contains (substring(x,5,size(x)-6)) #"." then (PUSH(ERROR(":error:"))::getpushstring(xs)) else commandlist(x)::getpushstring(xs) 

	fun makestring([],stack) = stack
		|makestring(INT x::xs,stack) = if x<0 then makestring(xs,"-"^substring(Int.toString(x),1,size(Int.toString(x))-1)::stack) else makestring(xs,Int.toString(x)::stack)
		|makestring(NAME x::xs,stack) = makestring(xs,x::stack)
		|makestring(ERROR x::xs,stack) = makestring(xs,x::stack)
		|makestring(STR x::xs,stack) = makestring(xs,x::stack)
		|makestring(BOOL x::xs,stack) = makestring(xs,x::stack)
		|makestring(UNIT x::xs,stack) = makestring(xs,x::stack)
		|makestring(FUN x ::xs,stack) = makestring(xs,x::stack)
	
	fun torfbinding([],t) = false
		| torfbinding((NAME(x),INT(y))::scope,t) = if x = t then true else torfbinding(scope,t)
		| torfbinding((NAME(x),UNIT(y))::scope,t) = if x = t then true else torfbinding(scope,t)
		| torfbinding((NAME(x),BOOL(y))::scope,t) = if x = t then true else torfbinding(scope,t)
		| torfbinding((NAME(x),STR(y))::scope,t) = if x = t then true else torfbinding(scope,t)
		| torfbinding((NAME(x),NAME(y))::scope,t) = if x = t then true else torfbinding(scope,t)

			(*to see if value is in scope*)
	fun firsttorf([],t) = false
		|firsttorf(x::xs,t) = if torfbinding(x,t) then true else firsttorf(xs,t)

	fun findvalue((NAME(x),UNIT(y))::scope,t) = if t=x then y else findvalue(scope,t)
		|findvalue((NAME(x),BOOL(y))::scope,t) = if t=x then y else findvalue(scope,t)
		|findvalue((NAME(x),STR(y))::scope,t) = if t=x then y else findvalue(scope,t)

			(*to get value*)
	fun findvaluefirst(altscope::scope,t)= if torfbinding(altscope,t) then findvalue(altscope,t) else findvaluefirst(scope,t)

		(*does int exist*)
	fun torfbindingint([],t) = false
		| torfbindingint((NAME(x),INT(y))::scope,t) = if x = t then true else torfbindingint(scope,t)
		| torfbindingint((x,y)::scope,t) = torfbindingint(scope,t)

			(*to see if its in the scope*)
	fun torfbindingbool([],t) = false
		| torfbindingbool((NAME(x),BOOL(y))::scope,t) = if x = t then true else torfbindingbool(scope,t)
		| torfbindingbool((x,y)::scope,t) = torfbindingbool(scope,t)

	fun findvaluebool((NAME(x),BOOL(y))::scope,t) = if t=x then y else findvaluebool(scope,t)
		|findvaluebool((x,y)::scope,t) = findvaluebool(scope,t)

		(*to get value of bool*)
	fun findvalueboolfirst(altscope::scope,t) = if torfbindingbool(altscope,t) then findvaluebool(altscope,t) else findvalueboolfirst(scope,t)

		(*to see if bool exists*)
	fun findbooltorf([],t) = false
		|findbooltorf(altscope::scope,t) = if torfbindingbool(altscope,t) then true else findbooltorf(scope,t)

			(*does int exist*)
	fun findinttorf([],t) = false
		|findinttorf(altscope::scope,t) = if torfbindingint(altscope,t) then true else findinttorf(scope,t)
	
	fun findvalueint((NAME(x),INT(y))::scope,t) = if t=x then y else findvalueint(scope,t)
		|findvalueint((x,y)::scope,t) = findvalueint(scope,t)

		(*to get value of int*)
	fun findvalueintfirst(altscope::scope,t) = if torfbindingint(altscope,t) then findvalueint(altscope,t) else findvalueintfirst(scope,t)

	fun needthis((NAME(x),y)::scope,t) = if x=t then y else needthis(scope,t)
 
 	(*getting back binding type*)
 	fun needthisfirst(altscope::scope,t) = if torfbinding(altscope,t) then needthis(altscope,t) else needthisfirst(scope,t)

	fun makenewlist([],t) = []
		|makenewlist((NAME(x),INT(y))::scope,t) = if x=t then makenewlist(scope,t) else (NAME(x),INT(y))::makenewlist(scope,t)
		|makenewlist((NAME(x),UNIT(y))::scope,t) = if x=t then makenewlist(scope,t) else (NAME(x),UNIT(y))::makenewlist(scope,t)
		|makenewlist((NAME(x),BOOL(y))::scope,t) = if x=t then makenewlist(scope,t) else (NAME(x),BOOL(y))::makenewlist(scope,t)
		|makenewlist((NAME(x),STR(y))::scope,t) = if x=t then makenewlist(scope,t) else (NAME(x),STR(y))::makenewlist(scope,t)

	fun final((QUIT::xs)::parent,mainstack::stack,scope,funstack,funscope,numfun,torf,tempfunc) = mainstack 
				| final((ADD::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(INT(t+h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((ADD::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) then final(xs::parent,(INT(t+findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((ADD::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) then final(xs::parent,(INT(h+findvalueintfirst(scope,t))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((ADD::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findinttorf(scope,t) then final(xs::parent,(INT(findvalueintfirst(scope,h)+findvalueintfirst(scope,t))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((ADD::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((ADD::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[ADD],sc)::tempfunc)
				| final((DIV::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h<>0 then final(xs::parent,(INT(t div h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((DIV::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findvalueintfirst(scope,h)<>0 then final(xs::parent,(INT(t div findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((DIV::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) andalso h<>0 then final(xs::parent,(INT(findvalueintfirst(scope,t) div h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((DIV::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findinttorf(scope,t) andalso findvalueintfirst(scope,h)<>0 then final(xs::parent,(INT(findvalueintfirst(scope,t) div findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((DIV::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((DIV::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[DIV],sc)::tempfunc)
				| final((MUL::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(INT(t*h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((MUL::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) then final(xs::parent,(INT(t*findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((MUL::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) then final(xs::parent,(INT(h*findvalueintfirst(scope,t))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((MUL::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findinttorf(scope,t) then final(xs::parent,(INT(findvalueintfirst(scope,h)*(findvalueintfirst(scope,t)))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((MUL::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((MUL::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[MUL],sc)::tempfunc)
				| final((REM::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h<>0 then final(xs::parent,(INT(t mod h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((REM::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findvalueintfirst(scope,h)<>0 then final(xs::parent,(INT(t mod findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((REM::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) andalso h<>0 then final(xs::parent,(INT(findvalueintfirst(scope,t) mod h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((REM::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findinttorf(scope,t) andalso findvalueintfirst(scope,h)<>0 then final(xs::parent,(INT(findvalueintfirst(scope,t) mod findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((REM::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((REM::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[REM],sc)::tempfunc)
				| final((SUB::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(INT(t - h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((SUB::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) then final(xs::parent,(INT(t-findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((SUB::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) then final(xs::parent,(INT(findvalueintfirst(scope,t)-h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((SUB::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findinttorf(scope,t) then final(xs::parent,(INT(findvalueintfirst(scope,t)-findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((SUB::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((SUB::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[SUB],sc)::tempfunc)
				| final((POP::xs)::parent,(h::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((POP::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((POP::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[POP],sc)::tempfunc) 
				| final((SWAP::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[SWAP],sc)::tempfunc)
				| final((SWAP::xs)::parent,(h::t::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(t::h::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((SWAP::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((NEG::xs)::parent,(INT(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h=0 then final(xs::parent,(INT(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(INT(~h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((NEG::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[NEG],sc)::tempfunc)
				| final((NEG::xs)::parent,(NAME(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) =  if findinttorf(scope,h) andalso findvalueintfirst(scope,h)=0 then final(xs::parent,(INT(findvalueintfirst(scope,h))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,h) andalso findvalueintfirst(scope,h)<>0 then final(xs::parent,(INT(~(findvalueintfirst(scope,h)))::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) 
				| final((NEG::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((PUSH i::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[PUSH(i)],sc)::tempfunc)
				| final((PUSH i::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(i::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)		
				| final((OR::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[OR],sc)::tempfunc)
				| final((OR::xs)::parent,(BOOL(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h=":false:" andalso t=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((OR::xs)::parent,(NAME(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,h) andalso t=":false:" andalso findvalueboolfirst(scope,h)=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,h) andalso t=":true:" orelse findvalueboolfirst(scope,h)=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((OR::xs)::parent,(BOOL(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,t) andalso h=":false:" andalso findvalueboolfirst(scope,t)=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,t) andalso h=":true:" orelse findvalueboolfirst(scope,t)=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::BOOL(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((OR::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,h) andalso findbooltorf(scope,t) andalso findvalueboolfirst(scope,h)=":false:" andalso findvalueboolfirst(scope,t)=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,h) andalso findbooltorf(scope,t) andalso findvalueboolfirst(scope,t)=":true:" orelse findvalueboolfirst(scope,h)=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((OR::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((AND::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[AND],sc)::tempfunc)
				| final((AND::xs)::parent,(BOOL(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h=":true:" andalso t=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((AND::xs)::parent,(NAME(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,h) andalso t=":true:" andalso findvalueboolfirst(scope,h)=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,h) andalso t=":false:" orelse findvalueboolfirst(scope,h)=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((AND::xs)::parent,(BOOL(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,t) andalso h=":true:" andalso findvalueboolfirst(scope,t)=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,t) andalso h=":false:" orelse findvalueboolfirst(scope,t)=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::BOOL(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((AND::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,h) andalso findbooltorf(scope,t) andalso findvalueboolfirst(scope,h)=":true:" andalso findvalueboolfirst(scope,t)=":true:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,h) andalso findbooltorf(scope,t) andalso findvalueboolfirst(scope,t)=":false:" orelse findvalueboolfirst(scope,h)=":false:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((NOT::xs)::parent,(BOOL(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h=":false:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((NOT::xs)::parent,(NAME(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findbooltorf(scope,h) andalso findvalueboolfirst(scope,h)=":true:" then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,h) andalso findvalueboolfirst(scope,h)=":false:" then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((NOT::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((NOT::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[NOT],sc)::tempfunc) 
				| final((Equal::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[Equal],sc)::tempfunc)
				| final((Equal::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h=t then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((Equal::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findvalueintfirst(scope,h)=t then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,h) andalso findvalueintfirst(scope,h)<>t then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)  else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((Equal::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) andalso findvalueintfirst(scope,t)=h then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,t) andalso findvalueintfirst(scope,t)<>h then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)  else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((Equal::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) andalso findinttorf(scope,h) andalso findvalueintfirst(scope,t)=findvalueintfirst(scope,h) then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,h) andalso findinttorf(scope,t) andalso findvalueintfirst(scope,t)<>findvalueintfirst(scope,h) then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((Equal::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((LessThan::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[LessThan],sc)::tempfunc)
				| final((LessThan::xs)::parent,(INT(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if h>t then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((LessThan::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,h) andalso findvalueintfirst(scope,h)>t then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,h) andalso findvalueintfirst(scope,h)<t then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((LessThan::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) andalso findvalueintfirst(scope,t)<h then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,t) andalso findvalueintfirst(scope,t)>h then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::INT(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((LessThan::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if findinttorf(scope,t) andalso findinttorf(scope,h) andalso findvalueintfirst(scope,t)<findvalueintfirst(scope,h) then final(xs::parent,(BOOL(":true:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findinttorf(scope,h) andalso findinttorf(scope,t) andalso findvalueintfirst(scope,t)>findvalueintfirst(scope,h) then final(xs::parent,(BOOL(":false:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((LessThan::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((IF::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[IF],sc)::tempfunc)
				| final((IF::xs)::parent,(h::t::BOOL(z)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if z=":true:" then final(xs::parent,(h::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(t::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((IF::xs)::parent,(h::t::NAME(z)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) =  if findbooltorf(scope,z) andalso findvalueboolfirst(scope,z)=":true:" then final(xs::parent,(h::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else if findbooltorf(scope,z) andalso findvalueboolfirst(scope,z)=":false:" then final(xs::parent,(t::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::h::t::NAME(z)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((IF::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((BIND::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[BIND],sc)::tempfunc)
				| final((BIND::xs)::parent,(INT(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = if torfbinding(altscope,t) then final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),INT(h))::makenewlist(altscope,t))::scope,funstack,funscope,numfun,false,tempfunc)  else final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),INT(h))::altscope)::scope,funstack,funscope,numfun,false,tempfunc)
				| final((BIND::xs)::parent,(BOOL(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = if torfbinding(altscope,t) then final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),BOOL(h))::makenewlist(altscope,t))::scope,funstack,funscope,numfun,false,tempfunc)  else final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),BOOL(h))::altscope)::scope,funstack,funscope,numfun,false,tempfunc)
				| final((BIND::xs)::parent,(UNIT(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = if torfbinding(altscope,t) then final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),UNIT(h))::makenewlist(altscope,t))::scope,funstack,funscope,numfun,false,tempfunc)  else final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),UNIT(h))::altscope)::scope,funstack,funscope,numfun,false,tempfunc)
				| final((BIND::xs)::parent,(STR(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = if torfbinding(altscope,t) then final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),STR(h))::makenewlist(altscope,t))::scope,funstack,funscope,numfun,false,tempfunc)  else final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),STR(h))::altscope)::scope,funstack,funscope,numfun,false,tempfunc)
				| final((BIND::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) =  if torfbinding(altscope,t) andalso torfbinding(altscope,h) then final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),needthis(altscope,h))::makenewlist(altscope,t))::scope,funstack,funscope,numfun,false,tempfunc) else if torfbinding(altscope,t)=false andalso torfbinding(altscope,h) then final(xs::parent,(UNIT(":unit:")::altstack)::stack,((NAME(t),needthis(altscope,h))::altscope)::scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)    
				| final((BIND::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((LET::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[LET],sc)::tempfunc) 
				| final((LET::xs)::parent,stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,[]::stack,[]::scope,funstack,funscope+1,numfun,false,tempfunc) 
				| final((END::xs)::parent,(alt::altstacktwo)::altstack::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,(alt::altstack)::stack,scope,funstack,funscope-1,numfun,false,tempfunc)
				| final((END::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[END],sc)::tempfunc) 
				| final((FUNDEF(FUN(i))::xs)::parent,stack,altsc::scope,altfs::funstack,funscope,numfun,false,tempfunc) = if funscope=0 then final(xs::parent,stack,altsc::scope,altfs::funstack,funscope,numfun+1,true,(FUNNAME(getname(i)),FUNARG(getarg(i)),[],altsc)::tempfunc) else final(xs::parent,stack,altsc::scope,[]::altfs::funstack,funscope,numfun+1,true,(FUNNAME(getname(i)),FUNARG(getarg(i)),[],altsc)::tempfunc)
				| final((FUNDEF(FUN(i))::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[FUNDEF(FUN(i))],sc)::tempfunc)
				| final((FUNEND::xs)::parent,altstack::stack,altscope::scope,altfs::funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = if numfun=1 then final(xs::parent,(UNIT(":unit:")::altstack)::stack,altscope::scope,((x,y,z,sc)::altfs)::funstack,funscope,numfun-1,false,tempfunc) else final(xs::parent,(UNIT(":unit:")::altstack)::stack,altscope::scope,altfs::funstack,funscope,numfun-1,true,(x,y,z@[FUNEND],sc)::tempfunc)				
				| final((CALL::xs)::parent,(NAME(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = if doesfunctorf(rev(funstack),funscope+1,h) andalso torfbinding(altscope,t) then final(meat(rev(funstack),h)::xs::parent,[]::altstack::stack,((NAME(getardbinding(funstack,h)),needthis(altscope,t))::turkey(rev(funstack),h))::altscope::scope,funstack,funscope+1,numfun,false,tempfunc) else if doesfunctorf(rev(funstack),funscope+1,h) andalso doesfunctorf(rev(funstack),funscope+1,t) andalso torfbinding(altscope,t)=false then final(meat(rev(funstack),h)::xs::parent,[]::altstack::stack,((NAME(getardbinding(funstack,h)),NAME(t))::turkey(rev(funstack),h))::altscope::scope,funstack,funscope+1,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::NAME(t)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc)
				| final((CALL::xs)::parent,(NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if doesfunctorf(rev(funstack),funscope+1,h) then final(meat(rev(funstack),h)::xs::parent,[]::altstack::stack,((NAME(getardbinding(funstack,h)),INT(t))::turkey(rev(funstack),h))::scope,funstack,funscope+1,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::INT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((CALL::xs)::parent,(NAME(h)::UNIT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) = if doesfunctorf(rev(funstack),funscope+1,h) then final(meat(rev(funstack),h)::xs::parent,[]::altstack::stack,((NAME(getardbinding(funstack,h)),UNIT(t))::turkey(rev(funstack),h))::scope,funstack,funscope+1,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::UNIT(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((CALL::xs)::parent,(NAME(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc) =  if doesfunctorf(rev(funstack),funscope+1,h) then final(meat(rev(funstack),h)::xs::parent,[]::altstack::stack,((NAME(getardbinding(funstack,h)),BOOL(t))::turkey(rev(funstack),h))::scope,funstack,funscope+1,numfun,false,tempfunc) else final(xs::parent,(ERROR(":error:")::NAME(h)::BOOL(t)::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((CALL::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[CALL],sc)::tempfunc) 
				| final((CALL::xs)::parent,altstack::stack,scope,funstack,funscope,numfun,torf,tempfunc) = final(xs::parent,(ERROR(":error:")::altstack)::stack,scope,funstack,funscope,numfun,torf,tempfunc)
				| final((RETURN::xs)::parent,(NAME(a)::alts)::altstack::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) = if torfbinding(altscope,a) then final(xs::parent,alts::(needthis(altscope,a)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) else final(xs::parent,alts::(NAME(a)::altstack)::stack,altscope::scope,funstack,funscope,numfun,false,tempfunc) 
				| final((RETURN::xs)::parent,(a::alts)::altstack::stack,scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,alts::(a::altstack)::stack,scope,funstack,funscope,numfun,false,tempfunc)
				| final((RETURN::xs)::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z,sc)::tempfunc) = final(xs::parent,stack,scope,funstack,funscope,numfun,true,(x,y,z@[RETURN],sc)::tempfunc)
				| final([]::xs::parent,xc::stack,x::scope,funstack,funscope,numfun,false,tempfunc) = final(xs::parent,stack,scope,funstack,funscope-1,numfun,false,tempfunc)

	fun tofile([]) = ( TextIO.closeIn inStream; TextIO.closeOut outStream)
		| tofile(x::xs) = (TextIO.output(outStream, x^"\n");tofile(xs));


in 
	tofile(rev(makestring(final(getpushstring(readfromfile(readLine))::[],[]::[],[]::[],[]::[],0,0,false,[]),[])))

end

(*val w = interpreter("/home/foreign/txtfiles/input.txt","/home/foreign/txtfiles/output.txt");*)
