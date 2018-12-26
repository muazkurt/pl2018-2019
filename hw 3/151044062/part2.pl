flight(ankara, diyarbakir, 8).
flight(ankara, istanbul, 2).
flight(ankara, izmir, 6).
flight(ankara, kars, 3).
flight(ankara, istanbul, 2).

flight(antalya, diyarbakir, 5).
flight(antalya, erzurum, 2).
flight(antalya, izmir, 1).

flight(diyarbakir, ankara, 8).
flight(diyarbakir, antalya, 5).

flight(erzurum, antalya, 2).
flight(erzurum, edirne, 5).

flight(edirne, erzurum, 5).

flight(gaziantep, kars, 3).

flight(istanbul, izmir, 3).
flight(istanbul, ankara, 2).
flight(istanbul, trabzon, 3).

flight(izmir, istanbul, 3).
flight(izmir, ankara, 6).
flight(izmir, antalya, 1).

flight(kars, ankara, 3).
flight(kars, gaziantep, 3).

flight(trabzon, istanbul, 3).
flight(trabzon, ankara, 6).


route(X, X, 0).
route(X, Y, C) :- 
    find(X, Y, [X], C).

sum(A, B, C) :- C is A + B.
find(X, X, _, C) :- C is 0.
%PS: is can be change as =
find(X, Y, List, C) :-
    flight(X, Z, Temp),
    not(member(Z, List)),
    find(Z, Y, [X|List], Temp1),
    sum(Temp, Temp1, C).