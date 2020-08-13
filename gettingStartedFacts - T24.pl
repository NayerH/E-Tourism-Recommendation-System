
% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin, 79).

% Solution:

possibleSubset([H|T],L) :- possibleSubset(T,P), insert(H,P,L).
possibleSubset([_|T],L) :- possibleSubset(T,L).
possibleSubset([],[]).

insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).





choosePreferences(Prefs, ChosenPrefs) :-
			possibleSubset(Prefs, TempChosenPrefs),
			member(activity(A), TempChosenPrefs),
			possibleSubset(A,A1),
			A1 \= [],
			replace(TempChosenPrefs, activity(A), activity(A1), ChosenPrefs).
choosePreferences(Prefs, ChosenPrefs) :-
			possibleSubset(Prefs, ChosenPrefs),
			\+ member(activity(_), ChosenPrefs).	
choosePreferences([], []).

replace([],_,_,[]).
replace([X|T],X,Y,[Y|T2]) :- replace(T,X,Y,T2).
replace([H|T],X,Y,[H|T2]) :- H \= X, replace(T,X,Y,T2).

member(H, [H|_]).
member(X, [H|T]) :- X \= H, member(X,T).





preferenceSatisfaction(Offer, Customer, ChosenPrefs, S) :- preferenceSatisfaction(Offer, Customer, ChosenPrefs, 0, S).
preferenceSatisfaction(_, _, [], Acc, Acc).
preferenceSatisfaction(O, C, [H|T], Acc, S) :- 
			H \= means(_), H \= accommodation(_), H \= activity(_),
			preferenceSatisfaction(O,C,T,Acc,S).
preferenceSatisfaction(O, C, [H|T], Acc, S) :- 
			((H = means(M), offerMean(O, M), customerPreferredMean(C, M, R)); 
			(H = accommodation(A), offerAccommodation(O, A), customerPreferredAccommodation(C, A, R));
			(H = activity(L), calculateRateActivity(O, C, L, R))),
			NewAcc is Acc + R,
			preferenceSatisfaction(O, C, T, NewAcc, S).

calculateRateActivity(_,_,[],0).
calculateRateActivity(O, C, [H|T],S):-
			O = offer(_, L, _, _, _, _, _, _),
			member(H, L),
			calculateRateActivity(O, C, T, NewS),
			customerPreferredActivity(C, H, R),
			S is NewS + R.	
calculateRateActivity(O, C, [H|T],S):-
			O = offer(_, L, _, _, _, _, _, _),
			\+ member(H, L),
			calculateRateActivity(O, C, T, S).





overlapPeriod(period(P1,P2), period(P3,P4)) :-
		P1 = Y11-M11-D11, P2 = Y12-M12-D12, P3 = Y21-M21-D21, P4 = Y22-M22-D22,
		Date1 is Y11*10000 + M11*100 + D11,
		Date2 is Y12*10000 + M12*100 + D12,
		Date3 is Y21*10000 + M21*100 + D21,
		Date4 is Y22*10000 + M22*100 + D22,
		((Date1 =< Date3, Date3 =< Date2);
		(Date1 >= Date3, Date4 >= Date1)).





getOffer(ChosenPrefs,O):- 
		offerMean(O,_),
		getOfferH(ChosenPrefs,O).
getOfferH([dest(Destination)|T], O) :- 
		O = offer(Destination, _, _, _, _, _, _, _), 
		getOfferH(T, O).
getOfferH([budget(BudgetPref)|T], O) :-
		O = offer(_, _, BudgetOriginal, _, _, _, _, _), 
		BudgetOriginal =< BudgetPref,
		getOfferH(T, O).
getOfferH([means(MeansPref)|T], O) :- 
		offerMean(O, MeansPref), 
		getOfferH(T, O).
getOfferH([accommodation(AccomPref)|T], O) :- 
		offerAccommodation(O, AccomPref), 
		getOfferH(T, O).
getOfferH([period(P1,P2)|T], O) :- 
		O = offer(_, _, _, _, _, period(P3,P4), _, _),
		overlapPeriod(period(P1,P2), period(P3,P4)),
		getOfferH(T, O).
getOfferH([activity([H|_])|T], O) :-
		O = offer(_, ActivityList, _, _, _, _, _, _),
		member(H, ActivityList),
		getOfferH(T, O).
getOfferH([activity([H|TActivity])|T], O) :-
		O = offer(_, ActivityList, _, _, _, _, _, _),
		\+ member(H, ActivityList),
		getOfferH([activity(TActivity)|T], O).
getOfferH([],_).





recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
		choosePreferences(Prefs, ChosenPrefs),
		getOffer(ChosenPrefs, O).