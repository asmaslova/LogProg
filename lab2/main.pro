﻿implement main
    open core, file, stdio

domains
    phname = столички; ромашка; низкоцен.

class facts - pharmaciesDb
    аптека : (integer IdАптеки, phname НазваниеАптеки, string Адрес, string Телефон).
    лекарство : (integer IdЛекарства, string НазваниеЛекарства).
    покупка : (integer IdПокупки, string Покупатель, integer IdАптеки).
    состав_покупки : (integer IdПокупки, integer IdЛекарства, integer Количество).
    продает : (integer IdАптеки, integer IdЛекарства, integer Цена, integer Количество).

class facts
    s : (real Sum) single.

clauses
    s(0).

class predicates
    min : (integer X, integer Y, integer Z [out]).
    max : (integer X, integer Y, integer Z [out]).

clauses
    min(X, Y, X) :-
        X <= Y,
        !.
    min(_, Y, Y).

    max(X, Y, X) :-
        X >= Y,
        !.
    max(_, Y, Y).

class facts
    stats : (integer Min, integer Max) single.

clauses
    stats(0, 0).

class predicates
    дешевле_чем : (integer IdЛекарства, integer MaxPrice) nondeterm.
    состав : (integer IdПокупки) nondeterm.
    наличие : (integer IdАптеки) nondeterm.
    сумма_покупки : (integer IdПокупки) nondeterm.
    %где_самое_дешевое : (integer IdЛекарства) nondeterm.

clauses
    состав(X) :-
        покупка(X, N, _),
        write("Клиент  ", N, " купил", ":\n"),
        состав_покупки(X, NP, A),
        лекарство(NP, NamePr),
        write("  ", NamePr, " ", A, "шт"),
        nl,
        fail.
    состав(X) :-
        покупка(X, _, _),
        write("Конец списка"),
        nl.

    наличие(X) :-
        аптека(X, NameA, Adr, _),
        продает(X, NL, _, Am),
        write("В аптеке ", NameA, " по адресу  ", Adr, " есть", ":\n"),
        лекарство(NL, NameL),
        write("  ", NameL, " ", Am, "шт"),
        nl,
        fail.
    наличие(X) :-
        аптека(X, _, _, _),
        write("Конец списка"),
        nl.

    сумма_покупки(X) :-
        покупка(X, _, IdP),
        assert(s(0)),
        состав_покупки(X, IdL, Am),
        продает(IdP, IdL, Pr, _),
        s(Sum),
        asserta(s(Sum + Am * Pr)),
        fail.
    сумма_покупки(X) :-
        покупка(X, Name, _),
        s(Sum),
        write("Клиент ", Name, " совершил покупку на сумму: ", Sum, " рублей"),
        nl.

    дешевле_чем(X, Y) :-
        лекарство(X, NameL),
        продает(IdP, X, Pr, _),
        аптека(IdP, NameP, Adr, _),
        if Pr < Y then
            write("Лекарство ", NameL, " дешевле ", Y, " рублей есть в аптеке ", NameP, " по адресу ", Adr, ".\n")
        end if.

    /*   где_самое_дешевое(X) :-
        лекарство(X, _NameL),
        продает(_IdP, X, Pr, _),
        assert(stats(100000, 0)),
        stats(MinP, _),
        if Pr < MinP then
            assert(stats(Pr, 0)),
            write(Pr, " ", MinP, nl)
        end if.

    где_самое_дешевое(X) :-
        stats(_Min, _Max),
        лекарство(X, NameL),
        аптека(_IdP, NameP, Adr, _),
        write("Лекарство ", NameL, " дешевле всего купить в аптеке ", NameP, " по адресу ", Adr, ".\n").
*/
    run() :-
        console::init(),
        reconsult("..\\pharms.txt", pharmaciesDb),
        состав(101),
        fail.
    run() :-
        наличие(21),
        сумма_покупки(101),
        дешевле_чем(6, 800),
        %где_самое_дешевое(3),
        fail.
    run() :-
        succeed.

end implement main

goal
    console::run(main::run).
