/* Examples for testing */
"\n** Examples for testing **\n\n";

true;
if false then true else false; 

x/;
x;

x = true;
x;
if x then false else x; 

lambda x. x;
(lambda x. x) (lambda x. x x); 

{x=lambda x.x, y=(lambda x.x)(lambda x.x)}; 
{x=lambda x.x, y=(lambda x.x)(lambda x.x)}.x; 

"hello";

timesfloat (timesfloat 2.0 3.0) (timesfloat 4.0 5.0);

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

let x=true in x;

/* *** Church booleans *** */
"\n** Church booleans **\n\n";

/* true and false */
t = lambda x. lambda y. x;
f = lambda x. lambda y. y;

/* ¬a */
not = lambda a. f t;

/* a ^ b (conjunction) */
and = lambda a. lambda b. a b a;

/* a v b (disjunction) */
or = lambda a. lambda b. a a b;

/* ¬a ^ b (implication ->) */
lambda a. lambda b. a f b;

/* conversion to and from native */
unchurch_bool = lambda b. b true false;
church_bool = lambda b. if b then t else f;

and (church_bool true) (church_bool false);
or (not (church_bool true)) (church_bool false);

/* *** Church numerals *** */
"\n** Church numerals **\n\n";

church_zero = lambda f. lambda x. x;
church_iszero = lambda n. n (lambda x. false) true;
church_succ = lambda n. lambda f. lambda x. f (n f x);

church_1 = church_succ church_zero;
church_2 = church_succ church_1;
church_3 = church_succ church_2;
church_4 = church_succ church_3;
church_5 = church_succ church_4;
church_6 = church_succ church_5;
church_7 = church_succ church_6;
church_8 = church_succ church_7;
church_9 = church_succ church_8;
church_plus = lambda m. lambda n. lambda f. lambda x. m f (n f x);
church_mult = lambda m. lambda n. lambda f. m (n f);

unchurch_int = lambda f. f (lambda x. succ x) 0;

/* 5 * 3 */
unchurch_int (church_mult church_5 church_3);

/* *** Recursive functions *** */
"\n** Recursive functions **\n\n";
intEq = rec inteq.
	lambda n1.
	lambda n2.
		if iszero n1
		then
			if iszero n2
			then true
			else false
		else
			if iszero n2
			then false
			else
				inteq
					(pred n1)
					(pred n2);

/* 5 == 12 */
intEq 5 12;

/* 13 == 7 */
intEq 13 7;

/* 10 == 10 */
intEq 10 10;

