
* `exp1.dta'
capture confirm variable educexp
if !_rc {
	global keeplist "educexp"
}
* 

* `exp2.dta'
capture confirm variable garb
if !_rc {
	global keeplist "watb1 watb2 elecb garb"
}
* 

* `exp3.dta'
capture confirm variable landexp
if !_rc {
	global keeplist "landexp"
}
* 

* `exp4.dta'
capture confirm variable cropexp
if !_rc {
	global keeplist "cropexp"
}
* 

* `exp5.dta'
capture confirm variable livexp
if !_rc {
	global keeplist "livexp"
}
* 

* `exp6.dta'
capture confirm variable fdprexp2
if !_rc {
	global keeplist "fdprexp1 fdprexp2"
}
* 

* `exp7.dta'
capture confirm variable hp
if !_rc {
	global keeplist "hp"
}
* 

* `exp8.dta'
capture confirm variable yrexp
if !_rc {
	global keeplist "yrexp"
}
* 

* `exp9.dta'
capture confirm variable dayexp
if !_rc {
	global keeplist "dayexp"
}
* 

* `exp10.dta'
capture confirm variable foodexp
if !_rc {
	global keeplist "foodexp"
}
* 

* `exp11.dta'
capture confirm variable nfinp
if !_rc {
	global keeplist "nfinp"
}
* 

* `exp12.dta'
capture confirm variable eqdepn
if !_rc {
	global keeplist "eqdepn"
}
* 

* `exp13.dta'
capture confirm variable assdepn
if !_rc {
	global keeplist "assdepn"
}
* 

* `exp14.dta'
capture confirm variable remitexp
if !_rc {
	global keeplist "remitexp"
}
* 

* `exp15.dta'
capture confirm variable miscexp
if !_rc {
	global keeplist "miscexp" 
}
* 

* `exp16.dta'
capture confirm variable useval
if !_rc {
	global keeplist "useval" 
}
*

* `exp17.dta'
capture confirm variable rentpaid
if !_rc {
	global keeplist "rentpaid" 
}
*

* `exp18.dta'
capture confirm variable imprtown
if !_rc {
	global keeplist "imprtown" 
}
*

* `exp19.dta'
capture confirm variable imprtpar
if !_rc {
	global keeplist "imprtpar" 
}
*

* `exp20.dta'
capture confirm variable imprtemp
if !_rc {
	global keeplist "imprtemp" 
}
*

* `exp21.dta'
capture confirm variable imprtsqu
if !_rc {
	global keeplist "imprtsqu"
}
*

* `inc1.dta' 
capture confirm variable schol
if !_rc {
	global keeplist "schol"
}
*

* `inc2.dta' 
capture confirm variable j1k
if !_rc {
	global keeplist "j1cash j1secash j1k"
}
*

* `inc3.dta' 
capture confirm variable j2k
if !_rc {
	global keeplist "j2cash j2secash j2k"
}
*

* `inc4.dta' 
capture confirm variable j3k
if !_rc {
	global keeplist "j3cash j3secash j3k"
}
*

* `inc5.dta' 
capture confirm variable j4k
if !_rc {
	global keeplist "j4cash j4secash j4k"
}
*

* `inc6.dta' 
capture confirm variable incwat
if !_rc {
	global keeplist "incwat"
}
*

* `inc7.dta' 
capture confirm variable inclnd2
if !_rc {
	global keeplist "inclnd1 inclnd2"
}
*

* `inc8.dta' 
capture confirm variable incliv
if !_rc {
	global keeplist "incliv"
}
*

* `inc9.dta' 
capture confirm variable inceq
if !_rc {
	global keeplist "inceq"
}
*

* `inc10.dta' 
capture confirm variable cropsv2
if !_rc {
	global keeplist "cropsv1 cropsv2"
}
*

* `inc11.dta' 
capture confirm variable rootsv
if !_rc {
	global keeplist "rootsv"
}
*

* `inc12.dta' 
capture confirm variable othaginc
if !_rc {
	global keeplist "othaginc"
}
*

* `inc13.dta' 
capture confirm variable inctrcrp
if !_rc {
	global keeplist "inctrcrp"
}
*

* `inc14.dta' 
capture confirm variable prnfund3
if !_rc {
	global keeplist "incnfrnt incnfc incnfk incnfdom prnfdom prnfund1 prnfund2 prnfund3"
}
*

* `inc15.dta' 
capture confirm variable increm
if !_rc {
	global keeplist "increm"
}
*

* `inc16.dta' 
capture confirm variable incmisc
if !_rc {
	global keeplist "incmisc"
}
*
