room(z06).
room(z11).
capacity(z06, 10).
capacity(z11, 10).
equipment(z06, hcapped).
equipment(z06, smartboar).
equipment(z11, hcapped).
equipment(z11, 'Projector').


course(cse321).
instructor(cse321, gozupek).
capacity(cse321, 10).
hour(cse321, 4).
room(cse321, z06).
%need(cse321, ).


course(cse331).
instructor(cse331, bayraktar).
capacity(cse331, 6).
hour(cse331, 3).
room(cse331, z11).

course(cse341).
instructor(cse341, genc).
capacity(cse341, 5).
hour(cse341, 3).
room(cse341, z06).

course(cse343).
instructor(cse343, turker).
capacity(cse343, 10).
hour(cse343, 4).
room(cse343, z11).
