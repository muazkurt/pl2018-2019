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

%flight(ankara, ankara, 0).
%flight(antalya, antalya, 0).
%flight(diyarbakir, diyarbakir, 0).
%flight(erzurum, erzurum, 0).
%flight(edirne, edirne, 0).
%flight(gaziantep, gaziantep, 0).
%flight(istanbul, istanbul, 0).
%flight(izmir, izmir, 0).
%flight(kars, kars, 0).
%flight(trabzon, trabzon, 0).

%route(X, Y, C) :- flight(X, Y, C).

listcontains(X, List) :- member(X, [])
route(X, Y, C) :- find(X, Y, [], C).
find(X, Y, List, C) :- 
    not(member(X, List)), 
    flight(X, Z, Temp1), 
    append(List, [X], List2), 
    find(Z, Y, List2, Temp2), 
    C = Temp1 + Temp2.