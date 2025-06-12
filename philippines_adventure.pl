/* phillippines adventure by Flora Dallinger, Lena Grassauer and Katharina Einzenberger */

:- dynamic i_am_at/1, at/2, holding/1, recipe/2, inside/2, cupboard_open/1, close_object/1.
:- discontiguous open_object/1.

introduction :- 
        write('The sea lies calm. Tropical heat shimmers over the wooden deck of your ship as the sun goes down. Gulls circle above the coast—white sand, dense jungle, smoke rising from the huts on the horizon. After months at sea… finally land.'), nl,
        write('The Philippines. A place not yet marked on your map. A distant edge of the world—and perhaps the end of your journey.'), nl,
        write('You are Ferdinand Magellan, captain of the last remaining ships of your fleet. On behalf of the Spanish crown, you have crossed the West—through storm, hunger, and mutiny. And now, you are here. Standing beside your loyal companion Uwentus.'), nl.

/* This rule just writes out game instructions. */

/* TODO: rewrite to actual commands*/
instructions :- 
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                  -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.  -- to go in that direction (north, south, east, west, up, down).'), nl,
        write('take(Object).           -- to pick up an object.'), nl,
        write('take_out(Object).       -- to take an object out of a drawer.'), nl,
        write('drop(Object).           -- to put down an object.'), nl,
        write('look.                   -- to look around you again.'), nl,
        write('open_object(Drawer).    -- to open a drawer or cupboard.'), nl,
        write('close_object(Drawer).   -- to close a drawer or cupboard; IMPORTANT: please remember to always close a drawer, or you will not be able to open it again.'), nl,
        write('recipes.                -- to show all recipes you are able to craft.'), nl,
        write('craft(Object).          -- to craft an object that you looked up in the recipe book; you have to hold the needed items.'), nl,
        write('instructions.           -- to see this message again.'), nl,
        write('halt.                   -- to end the game and quit.'), nl.

/* start */
start :- 
        retractall(i_am_at(_)),
        assert(i_am_at(boat_deck)),
        retractall(holding(_)),
        introduction, 
        instructions,
        nl,
        look.
        
/* define first location */

i_am_at(boat_deck).

/* These facts tell where the various objects in the game
   are located. */

at(wood, pantry).
at(barrels, pantry).
at(cupboard, captains_cabin_cupboard).
at(flint, cupboard).

/* describe what objects are inside something */

inside(flint, cupboard).

/* rule to define what can be opened */
can_be_opened(cupboard).
cupboard_open(false).

/* rule to open something */
open_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        can_be_opened(Object),
        cupboard_open(false),
        retract(cupboard_open(false)),
        assert(cupboard_open(true)),

        % Finde alle Items, die sich aktuell im Objekt befinden
        findall(Item, inside(Item, Object), Items),

        % Inhalt anzeigen oder sagen, dass es leer ist
        ( Items = [] ->
                write('It is empty.'), nl
        ;
                write('You open the '), write(Object),
                write(' and find: '), write(Items), nl
        ),
        !.

open_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        (\+ can_be_opened(Object); cupboard_open(true)),
        write(Object), write(' cannot be opened.'), nl,
        !.

open_object(_) :-
        write('There is nothing like that to open here.'), nl,
        !.


/* rule to close something */
close_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        can_be_opened(Object),
        cupboard_open(true),
        retract(cupboard_open(true)),
        assert(cupboard_open(false)),
        !.

close_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        \+ can_be_opened(Object),
        write(Object), write(' cannot be closed.'), nl,
        !.

close_object(_) :-
        write('There is nothing like that to close here.'), nl,
        !.

/* describe how the locations are connected */

path(boat_deck, e, beach).
path(boat_deck, w, captains_cabin).
path(boat_deck, d, pantry).

path(beach, w, boat_deck).
path(beach, e, jungle).
path(beach, s, river).

path(captains_cabin, e, boat_deck).
path(captains_cabin, n, captains_cabin_cupboard).

path(captains_cabin_cupboard, s, captains_cabin).

path(pantry, u, boat_deck).

path(river, n, beach).

path(jungle, w, beach).
path(jungle, e, friend_village).

path(friend_village, s, enemy_village).



/* These facts tell what you can craft out of objects */

recipes :- 
    recipe(New, X, Y),
    write('To craft '), write(New),
    write(', you need '), write(X),
    write(' and '), write(Y), nl,
    fail.
recipes.  % succeeds after all options fail

recipe(torch, flint, wood).

/* This rule tells how to look around you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        list_inventory,
        !.

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* These rules describe how to take an object out of a drawer */

take_out(X) :-
         holding(X),
        write('You''re already holding it!'),
        !, nl.

take_out(X) :-
        i_am_at(captains_cabin_cupboard),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take_out(_) :-
        write('I don''t see it here.'),
        nl.

        /* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'), nl,
        !.

drop(_) :-
        write('You aren''t holding it!'),
        nl,
        !.

/* These rules define the six direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).

/* This rule is how you move */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        !,
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look,
        nl.

go(_) :-
        write('You can''t go that way.').



/* These rules craft an object */
craft(New) :- 
        recipe(New, X, Y),
        holding(X),
        holding(Y),
        retract(holding(X)),
        retract(holding(Y)),
        assert(holding(New)),
        write('Holding new object '), write(New),
        !, nl.

craft(_) :- 
        write('You cannot craft such an object or don\'t have the ressources for it'), nl.


/* This rule lists all of your elements */
list_inventory :-
        findall(Item, holding(Item), Items),
        ( Items = [] ->
            write('You are not holding anything.'), nl
        ; 
            write('You are holding: '), write(Items), nl
        ).
        
/* These rules describe the various rooms. */

describe(boat_deck) :- write('You are currently on the deck of the last remaining boat of your fleet. You have a broad deck, two stairs and the entry to the captains cabin.'), nl.
describe(captains_cabin) :- write('You now have entered the cabin of the captain... you.'), nl.
describe(pantry) :- write('You are currently in the pantry where food and drinks are stored.'), nl.
describe(beach) :- write('You are standing on the beach after you left the boat. You hear seaguls screeching and the waves crushing in. On the north you see the beginning of a gigantic wood. You can hear the monkey  Uwentus is right behind you.'), nl.
describe(captains_cabin_cupboard) :- write('You are standing in front of a cupboard in your cabin.'), nl.


/* rules for describing which objects are around player */

notice_objects_at(boat_deck) :- write('You find nothing around you except the wide sea and your faithful friend Uwentus'),nl.
notice_objects_at(captains_cabin) :- write('It\'s dark and stuffy in here. Around you there are some cupboards and drawers.'),nl.
notice_objects_at(pantry) :- write('You find some dusty barrels lying around. Behind them you can see some wood.'), nl.
notice_objects_at(beach) :- write('The beach is relatively empty. Some stones and shells lying around... nothing special'), nl.
notice_objects_at(captains_cabin_cupboard) :- write('The cupboard is closed. You have to open it before you can access anything inside.'), nl.

die :- 
        write('You died. Maybe try again and choose a different ending.'), nl,
        halt.

sail_away :-
        i_am_at(boat_deck),
        write('Congratulations! You did the only right thing to do in this situation. You sailed away and left the people of the foreign island live their life in peace.'),nl,
        halt.
sail_away :-
        write('You have to be on the boat deck to sail away.'), nl.        
