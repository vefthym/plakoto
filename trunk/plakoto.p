:- import random/3 from random.
:- import select/3 from basics.
:- import append/3 from basics.
:- import datime_setrand/0 from random.
:- import member/2 from basics.
:- import memberchk/2 from basics.
:- import ith/3 from basics.
:- import abs/1 from basics.
:- import reverse/2 from basics.
:- import length/2 from basics.
:- dynamic turn/1.
:- dynamic board/1.
:- dynamic user/1.
:- dynamic pc/1.
:- dynamic poss/1.
:- dynamic zaria/1.
:- dynamic ratings/1.
:- dynamic ind/1.
:- dynamic com/1.
:- dynamic deck/1.
:- dynamic points/1.

%roll one dice
roll_dice(X) :- random(1,7,X).

%roll the 2 dice	
dice(D1, D2) :- roll_dice(D1), roll_dice(D2), 
		retractall(zaria(_)), assert(zaria([])), zaria(Z), retractall(zaria(_)),
		(D1 = D2 -> append(Z, [D1], Z2),append(Z2, [D1], Z3),
			    append(Z3, [D1], Z4),append(Z4, [D1], Z5), assert(zaria(Z5))
		;append(Z, [D1], Z2),append(Z2, [D2], Z3), assert(zaria(Z3))).


%player Player plays first, with dice D1, D2
%first_turn(Player, D1, D2) :- dice(D1, D2), (D1 > D2 -> Player = user, assert(turn(user));
%			D1 < D2, Player = pc, assert(turn(pc)) ; 
%			first_turn(Player, D1, D2)).

%player Player plays first, with dice D1, D2
first_turn(Player, D1, D2) :- dice(D1, D2), D1 > D2, Player = user, assert(turn(user)).
first_turn(Player, D1, D2) :- dice(D1, D2), D1 < D2, Player = pc, assert(turn(pc)).
first_turn(Player, D1, D2) :- first_turn(Player, D1, D2).
  

%first and last position of board used for collected checkers
initialize_board :- retractall(board(_)), 
	assert(board([0, 
	user, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, pc, 
	0])), retractall(user(_)), 
	assert(user([2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2])), retractall(pc(_)), 
	assert(pc([25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25])), assert(poss([])),
	assert(ratings([])).


%orismos portas
porta(user, I, U, _, _) :- ith(X, U, I), ith(Y, U, I), \+(X = Y).
porta(user, I, _, Pc, B) :- ith(I, B, user), memberchk(I, Pc).
porta(pc, I, _, Pc, _) :- ith(X, Pc, I), ith(Y, Pc, I), \+(X = Y).
porta(pc, I, U, _, B) :- ith(I, B, pc), memberchk(I, U).


write_board([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26],U,P) :-
	nl,nl,
	write('1 (collected pc): '),write(X1),nl,
	write('|2	|'),write('3	|'),write('4	|'),write('5	|'),write('6	|'),write('7	|'),
	write('8	|'),write('9	|'),write('10	|'),write('11	|'),write('12	|'),write('13	'),nl,
	write('|'),write(X2),write('	|'),write(X3),write('	|'),write(X4),write('	|'),write(X5),write('	|'),
	write(X6),write('	|'),write(X7),write('	|'),write(X8),write('	|'),write(X9),write('	|'),
	write(X10),write('	|'),write(X11),write('	|'),write(X12),write('	|'),write(X13),nl,nl,nl,write('|'),
	write(X25),write('	|'),write(X24),write('	|'),write(X23),write('	|'),write(X22),write('	|'),
	write(X21),write('	|'),write(X20),write('	|'),write(X19),write('	|'),write(X18),write('	|'),
	write(X17),write('	|'),write(X16),write('	|'),write(X15),write('	|'),write(X14),nl,
	write('|25	|'),write('24	|'),write('23	|'),write('22	|'),write('21	|'),write('20	|'),
	write('19	|'),write('18	|'),write('17	|'),write('16	|'),write('15	|'),write('14	'),nl,
	write('26 (collected user): '),write(X26),nl,nl,
	mergesort(U, NewU), mergesort(P, NewP1), reverse(NewP1, NewP),
	write('user:	'), write(NewU), nl,
	write('pc:	'), write(NewP), nl,nl.


write_all :- user(U), pc(P),
	board([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26]),
	nl,nl,
	write('1 (collected pc): '),write(X1),nl,
	write('|2	|'),write('3	|'),write('4	|'),write('5	|'),write('6	|'),write('7	|| '),
	write('8	|'),write('9	|'),write('10	|'),write('11	|'),write('12	|'),write('13	'),nl,
	write('|'),write(X2),write('	|'),write(X3),write('	|'),write(X4),write('	|'),write(X5),write('	|'),
	write(X6),write('	|'),write(X7),write('	|| '),write(X8),write('	|'),write(X9),write('	|'),
	write(X10),write('	|'),write(X11),write('	|'),write(X12),write('	|'),write(X13),nl,nl,nl,write('|'),
	write(X25),write('	|'),write(X24),write('	|'),write(X23),write('	|'),write(X22),write('	|'),
	write(X21),write('	|'),write(X20),write('	|| '),write(X19),write('	|'),write(X18),write('	|'),
	write(X17),write('	|'),write(X16),write('	|'),write(X15),write('	|'),write(X14),nl,
	write('|25	|'),write('24	|'),write('23	|'),write('22	|'),write('21	|'),write('20	|| '),
	write('19	|'),write('18	|'),write('17	|'),write('16	|'),write('15	|'),write('14	'),nl,
	write('26 (collected user): '),write(X26),nl,nl,
	mergesort(U, NewU), mergesort(P, NewP1), reverse(NewP1, NewP),
	write('user:	'), write(NewU), nl,
	write('pc:	'), write(NewP), nl,nl.

mergesort([],[]).    
mergesort([A],[A]).
mergesort([A,B|R],S) :-  
   split([A,B|R],L1,L2),
   mergesort(L1,S1),
   mergesort(L2,S2),
   merge(S1,S2,S).

split([],[],[]).
split([A],[A],[]).
split([A,B|R],[A|Ra],[B|Rb]) :-  split(R,Ra,Rb).

merge(A,[],A).
merge([],B,B).
merge([A|Ra],[B|Rb],[A|M]) :-  A =< B, merge(Ra,[B|Rb],M).
merge([A|Ra],[B|Rb],[B|M]) :-  A > B,  merge([A|Ra],Rb,M).

%insert_ith(+Index, +List, +Value, ?NewList)
insert_ith(N, [H|T], Value, NewList) :-
	(N=:=1 -> NewList = [Value|T]
	; NewList = [H|Rest1], N1 is N-1, insert_ith(N1, T, Value, Rest1)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% KANONES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%update_src(+Index, +List, ?NewBoardList)
update_src(Player, Src, U, Pc, B, NewB) :- (Player = user -> 
		(memberchk(Src, U) -> NewB = B
		;	(memberchk(Src, Pc) -> insert_ith(Src, B, pc, NewB) 
	  		;insert_ith(Src, B, 0, NewB)
			)
		)
	;(memberchk(Src, Pc) -> NewB = B
		;	(memberchk(Src, U) -> insert_ith(Src, B, user, NewB) 
	  		;insert_ith(Src, B, 0, NewB)
			)
		)
	). 

%check if Src belongs to Player
check_src(Player, Src, B) :- (Player = user -> ith(Src,B,user)
				     ; ith(Src, B, pc)).

%check if Dst is not enemy porta
check_dst(Player, Dst, U, Pc, B) :- Dst < 27, Dst > 0, (Player = user -> \+porta(pc, Dst, U, Pc, B)
					      ; \+porta(user, Dst, U, Pc, B)).

%check direction
check_direction(Player, Src, Dst) :- (Player = user -> Dst>Src ; Src>Dst).

%check distance
check_dist(Src, Dst, D) :- abs(Dst-Src) =:= D. 

%check if Player can collect
check_collective(Player, U, P) :- (Player = user -> (member(X, U), X < 20 -> fail; 1=1)
			; (member(X, P), X > 7 -> fail; 1=1)).

check_collective2(Player, Src, U, P, D) :- (Player = user -> (member(X, U), X < Src, D > 26-Src -> fail; 1=1)
			; (member(X, P), X > Src, D > Src-1 -> fail; 1=1)).

valid_move(Player, Src, Dst, D, U, Pc, B) :- check_direction(Player, Src, Dst), check_src(Player, Src, B), 
				check_dst(Player, Dst, U, Pc, B),  
				(Dst = 26 -> check_collective(Player, U, Pc), check_collective2(Player, Src, U, Pc, D)
					; (Dst = 1 -> check_collective(Player, U, Pc))
					; check_dist(Src, Dst, D)
				). 

%find possible moves for a current board and store them in poss([])
possible_moves(_,_,_,_,[],Prefix,_) :- poss(P), retractall(poss(_)),					 
					append(P, [Prefix], NewP), assert(poss(NewP)).
possible_moves(_, _, _, _,_,Prefix,26) :- (Prefix = [] ->1=1; poss(P), retractall(poss(_)),   
					append(P, [Prefix], NewP), assert(poss(NewP))).
possible_moves(Player, U, Pc, B, [H|T], Prefix, Counter) :- (Player = user -> 
		 Dst1 is Counter+H, (Dst1 > 26 -> Dst2 is 26; Dst2 = Dst1), 
		 (valid_move(user, Counter, Dst2, H, U, Pc, B), length([H|T], A) -> 
			append(Prefix, [[Counter, Dst2]], NewPrefix),
			check_move(user, Counter, Dst2, U, Pc, B, NewU, Pc, NewB), 
			possible_moves(Player, NewU, Pc, NewB, T, NewPrefix, 1), C1 is Counter+1,
			possible_moves(Player, U, Pc, B, [H|T], Prefix, C1)
			; Counter1 is Counter+1, possible_moves(Player, U, Pc, B, [H|T], Prefix, Counter1))
	; Dst1 is Counter-H, (Dst1 < 1 -> Dst2 is 1; Dst2 = Dst1),
		 (valid_move(pc, Counter, Dst2, H, U, Pc, B), length([H|T], A) -> 
			append(Prefix, [[Counter, Dst2]], NewPrefix),			
			check_move(pc, Counter, Dst2, U, Pc, B, U, NewPc, NewB),
			possible_moves(Player, U, NewPc, NewB, T, NewPrefix, 1), C1 is Counter+1,
			possible_moves(Player, U, Pc, B, [H|T], Prefix, C1)
			; Counter1 is Counter+1, possible_moves(Player, U, Pc, B, [H|T], Prefix, Counter1))
	).

find_moves(Player, U, Pc, B, Z, Counter) :- possible_moves(Player, U, Pc, B, Z, [], Counter), reverse(Z, RZ), 
					    possible_moves(Player, U, Pc, B, RZ, [], Counter).

restrict_moves(Moves, Max) :- list_max(Moves, Max), cut_moves(Moves, Max). 

cut_moves([],_).
cut_moves([H|T], Max) :-  length(H, L), (L = Max ->  poss(P), 
				retractall(poss(_)), append(P, [H], NewP), assert(poss(NewP));1=1),
				cut_moves(T, Max). 


%return the sublist with the greatest length
list_max([_],1).
list_max([X|Tail],M):-list_max(Tail,M1),max(X,M1,M).
max(X,Y,A):-length(X, A),A>Y.
max(X,Y,Y):-length(X, A),A=<Y.

%the main rules of the game
execute_user_move(D1, D2) :- (game_over-> write('Thank you!')
	; write('user turn: '), write(D1), write(', '), write(D2), nl, user(U), pc(Pc),board(B),zaria(Z), 
		(D1 = D2 -> possible_moves(user,U,Pc,B,Z,[],1),poss(Moves),
			retractall(poss(_)), assert(poss([])),
			(Moves = [] -> write('No possible moves!'),nl
			; restrict_moves(Moves, Max), poss(NewMoves), 
				retractall(poss(_)), assert(poss([])),
				read_and_move(NewMoves, [], Max))
		;find_moves(user,U,Pc,B,Z,1),poss(Moves),retractall(poss(_)), assert(poss([])),
			(Moves = [] -> write('No possible moves!'),nl
			; restrict_moves(Moves, Max), poss(NewMoves), 
				retractall(poss(_)), assert(poss([])),
				read_and_move(NewMoves, [], Max)) 
		), 
	retract(turn(user)), assert(turn(pc)), dice(D3, D4), execute_pc_move(D3, D4)
	).

read_and_move(Moves, _, 0) :- write('invalid moves! Try again!'),nl,zaria(Z),length(Z, A),
				read_and_move(Moves, [], A). 
read_and_move(Moves, UserMoves, Counter) :- write('move from:'), read(X), write('to:'),read(Y),
				append(UserMoves, [[X,Y]], NUserMoves), (member(NUserMoves,Moves) -> 
				move_list(user, NUserMoves)
				;Counter1 is Counter-1, read_and_move(Moves, NUserMoves, Counter1)).

execute_pc_move(D1, D2) :- (game_over-> write('Thank you!')
	; write('pc turn: '), write(D1), write(', '), write(D2), nl, user(U), pc(Pc),board(B),zaria(Z), 
	 (D1 = D2 -> possible_moves(pc,U,Pc,B,Z,[],1),poss(Moves),
		retractall(poss(_)), assert(poss([]))		
		;find_moves(pc,U,Pc,B,Z,1),poss(Moves),retractall(poss(_)), assert(poss([]))		
	 ), 
	restrict_moves(Moves, _), poss(NewMoves), retractall(poss(_)), assert(poss([])), 
	(NewMoves = [] -> write('No possible moves!'),nl

	;select_move(NewMoves, Best),write('pc moved: '), 
			write(Best),nl, move_list(pc, Best)),

	retract(turn(pc)), assert(turn(user)), dice(D3, D4), execute_user_move(D3, D4) 
	).

check_move(Player, Src, Dst, U, Pc, B, NewU, NewPc, NewB) :- (Player = user ->
				select(Src, U, Rest), append(Rest, [Dst], NewU), 
				insert_ith(Dst, B, user, NewB1), update_src(Player, Src, NewU, Pc, NewB1, NewB)
			;select(Src, Pc, Rest), append(Rest, [Dst], NewPc), 
			insert_ith(Dst, B, pc, NewB1), update_src(Player, Src, U, NewPc, NewB1, NewB)
			).

move_list(_, []) :- write_all.
move_list(Player, [H|T]) :- H = [Src,Dst], (Player = user -> move(user, Src, Dst), move_list(user, T)
			    ; move(pc, Src, Dst), move_list(pc, T)).  

%check if move is valid, update user,board and turn
move(Player, Src, Dst) :- user(U), pc(Pc), board(B), (Player = user -> select(Src, U, Rest), 
	append(Rest, [Dst], NewU), retractall(user(_)), assert(user(NewU)), 
	insert_ith(Dst, B, user, NewB1), update_src(Player,Src, NewU, Pc, NewB1, NewB2), 
	retractall(board(_)), assert(board(NewB2)), retractall(poss(_)), assert(poss([]))
	;select(Src, Pc, Rest),
	append(Rest, [Dst], NewPc), retractall(pc(_)),  assert(pc(NewPc)), 
	insert_ith(Dst, B, pc, NewB1), update_src(Player, Src, U, NewPc, NewB1, NewB2), 
	retractall(board(_)), assert(board(NewB2)), retractall(poss(_)), assert(poss([]))
	).


new_game :- write('New game!!!'),nl, initialize_board, datime_setrand, retractall(turn(_)), first_turn(Player,D1,D2), 
	write('user: '),write(D1),nl,write('pc: '), write(D2), write_all, 
	(Player = user -> execute_user_move(D1, D2) ; execute_pc_move(D1, D2)).

collected(pc, X) :- pc(L), count_list(L, X, 1).
collected(user, X) :- user(L), count_list(L, X, 26). 

count_list([],0,_).
count_list([X|T],Sum, Pos) :- count_list(T,Sum1,Pos), (X = Pos -> Sum is (Sum1+1); Sum is Sum1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRATEGY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rate(Moves, Rating) :- 	pc(Pc), board(B), test_move_list(Moves, Pc, B), rate_deck(Rating).

rate_deck(Rating) :- deck(D), user(U), com(C), retractall(points(_)), assert(points(0)),find_bars(D, U, C, 25), 
		 find_plakoma(D, U, 25) ,find_singles(D, U, C, 25), plakoma_manas(D, U), mana_single(D, U, C),
		 amyna(D, U, C), plakoma_perioxi(D, U, 7), rate_mazema(C), points(P), Rating is P.

find_bars(_,_,_,2).
find_bars(D, U, C, Counter):- (ith(X, C, Counter), ith(Y, C, Counter), \+(X = Y), \+member(Counter, U)-> 
				points(P), NewP is P + 3, retractall(points(_)), assert(points(NewP));1=1), 
				Counter1 is Counter-1, find_bars(D, U, C,Counter1).

find_plakoma(_, _, 2).
find_plakoma(D, U, Counter) :- (ith(Counter, D, pc), memberchk(Counter, U)-> 
				points(P), NewP is P + 5, retractall(points(_)), assert(points(NewP));1=1), 
				Counter1 is Counter-1, find_plakoma(D, U, Counter1).

find_singles(_,_,_, 2).
find_singles(D, U, C, Counter) :- (member(Counter, C), \+porta(pc,Counter, U, C, D) -> 
				points(P), NewP is P - 1, retractall(points(_)), assert(points(NewP));1=1), 
				Counter1 is Counter-1, find_singles(D, U, C, Counter1).

single_perioxis(_,_,_, 20).
single_perioxis(D, U, C, Counter) :- (member(Counter, C), \+porta(pc,Counter, U, C, D) -> 
				points(P), NewP is P - 3, retractall(points(_)), assert(points(NewP));1=1), 
				Counter1 is Counter-1, find_singles(D, U, C, Counter1).

plakoma_manas(D, U) :- (ith(2, D, pc), memberchk(2, U) ->
				points(P), NewP is P + 200, retractall(points(_)), assert(points(NewP));1=1).

plakoma_perioxi(_, _, 3).
plakoma_perioxi(D, U, Counter) :- (ith(Counter, D, pc), memberchk(Counter, U)-> 
				points(P), NewP is P + 11, retractall(points(_)), assert(points(NewP));1=1), 
				Counter1 is Counter-1, plakoma_perioxi(D, U, Counter1).

mana_single(D, U, C) :- (member(25, C), \+porta(pc,25, U, C, D) -> 
				points(P), NewP is P - 20, retractall(points(_)), assert(points(NewP));1=1).

anoigma(_,_,_, 20).
anoigma(D, U, C, Counter) :- (member(Counter, C), \+porta(pc,Counter, U, C, D) -> 
				points(P), NewP is P + 2, retractall(points(_)), assert(points(NewP));1=1), 
				Counter1 is Counter-1, anoigma(D, U, C, Counter1).

amyna(D, U, C) :- (ith(X, D, user), X > 11 -> single_perioxis(D, U, C, 25);anoigma(D, U, C, 25)).

rate_mazema(C) :-  test_collected(C, N), points(P), NewP is (P + N*4), retractall(points(_)), assert(points(NewP)).

test_collected(C, X) :- count_list(C, X, 1).

load_rate([]).
load_rate([H|T]) :-  rate(H, Rat),ratings(R), retractall(ratings(_)),append(R, [Rat], NewR),
			assert(ratings(NewR)), load_rate(T).

select_move(Moves, Best) :- load_rate(Moves), ratings(R), retractall(ratings(_)), 
			    assert(ratings([])), rate_list_max(R, _,1), ind(I), ith(I, Moves, Best).

rate_list_max([X],X,I) :- retractall(ind(_)),assert(ind(I)).
rate_list_max([X|Tail],M, I):- I1 is I+1, rate_list_max(Tail,M1, I1), 
					rate_max(X,M1,M, I1).
rate_max(X,Y,X,I):- X>Y,I1 is I-1,retractall(ind(_)),assert(ind(I1)).
rate_max(X,Y,Y,_):- X=<Y.

%tests a moves list and stores it in deck/1 and com/1
test_move_list([],Pc,B) :- retractall(com(_)), assert(com(Pc)), retractall(deck(_)), assert(deck(B)).
test_move_list([H|T], Pc, B) :- H = [Src,Dst], user(U), check_move(pc, Src, Dst, U, Pc, B, _, NewPc, NewB), 
				test_move_list(T, NewPc, NewB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GAME OVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pc_wins :- collected(pc, 15), collected(user, X), X > 0.
pc_wins_double :- collected(pc, 15), collected(user, 0).
pc_wins_double :- board(B), ith(2, B, pc), user(U), memberchk(2, U).
user_wins :- collected(user, 15), collected(pc, X), X > 0.
user_wins_double :- collected(user, 15), collected(pc, 0).
user_wins_double :- board(B), ith(25, B, user), pc(Pc), memberchk(25, Pc). 

game_over :- pc_wins, write('Game Over! Pc won.'),nl.
game_over :- pc_wins_double, write('Game Over! Pc won by double score!'),nl.
game_over :- user_wins, write('Game Over! User won.'),nl.
game_over :- user_wins_double, write('Game Over! User won by double score!'),nl.
