% Proyecto: N-Reinas algoritmo genetico
% EIF-400 Paradigmas de Programación
% Autor:
% Luis Venegas Ulloa

-module(nreinas).
-export([population_generator/1,population_fitness/1, max_population_fitness/1,crossover/3,board_max_population_fitness/1,
selection_ranges/1,array_probabilities/1,select_individual/2,new_generation/2,best_pos_fitness/1
,geneticosNReinas/1,validar_vec/1,individual_exists/4,insert_at/3,create_mtx/2,insertar_mtx/3,
insert_at_mtx/3]).

% Dominio: Una lista, una posicion de la lista.
% Codominio: Una lista.
split([],_N)->[];
split([H|T],N)-> split([H],T,N-1).
split(_L,[],_N)->[];
split(L,[H|T],1)->[L++[H]|T];
split(L,[H|T],N)-> split(L++[H],T,N-1).

% Dominio: ELemento a insertar, una lista, la posicion. 
% Codominio: Una lista.
insert_at(_P,[],_N)->[];
insert_at(P,[_H|T],1)-> [P]++T;
insert_at(P,[H|T],N)-> [H] ++ insert_at(P,T,N-1).

% Dominio: Una listas de listas, fila, columna.
% Codominio: Una lista de listas.
insert_at_mtx([],_F,_C)->[];
insert_at_mtx([H|T],1,C)-> [insert_at(1,H,C)]++T;
insert_at_mtx([H|T],F,C)-> [H] ++ insert_at_mtx(T,F-1,C).

% Dominio: Una listas de listas, fila, columna.
% Codominio: Una lista de listas.
insertar_mtx(M,[],_C)->M;
insertar_mtx(M,[H|T],C)->insertar_mtx(insert_at_mtx(M,H,C),T,C+1).

% Dominio: Una lista.
% Codominio: Suma de los elementos de una lista.
sum(L) -> sum(L, 0).
sum([H|T], Acc) -> sum(T, H + Acc); 
sum([], Acc) ->Acc.

% Dominio: Tamaño de la matriz.
% Codominio: Una lista.
create_mtx_aux(0)->[];
create_mtx_aux(N)-> [0]++ create_mtx_aux(N-1).

% Dominio: Fila y columna.
% Codominio: Una lista de listas.
create_mtx(_N,0)->[];
create_mtx(N,I)->[create_mtx_aux(N)]++create_mtx(N,I-1).

% Dominio: Rango de 0 a N y número de iteraciones.
% Codominio: Una lista.
create_list(_N,0)->[];
create_list(N,I)->[rand:uniform(N)]++create_list(N,I-1).

% Dominio: Filas, elemento a insertar y cantidad de columnas.
% Codominio: Una lista de listas.
create_lists(0,_Elemento,_I)->[];
create_lists(N,Elemento,I)->[Elemento|create_lists(N-1,create_list(I,I),I)].

% Dominio: N siendo el tamaño de la matriz nxn.
% Codominio: Una lista de listas.
population_generator(N)->create_lists(N*2,create_list(N,N),N).

% Dominio: Posición de la columna y una lista.
% Codominio: Cantidad de choques que se producen en una fila.
validate_row(_N,[])->0;
validate_row(N,[H|T])when N =:= H -> 1 + validate_row(N,T);
validate_row(N,[_H|T])-> validate_row(N,T).

% Dominio: Posición de la columna y una lista.
% Codominio: Cantidad de choques que se producen en una diagonal superior.
validate_upper_diagonal(_N,[])->0;
validate_upper_diagonal(N,[H|T])when N+1 =:= H ->1 + validate_upper_diagonal(N+1,T);
validate_upper_diagonal(N,[_H|T])-> validate_upper_diagonal(N+1,T).

% Dominio: Posición de la columna y una lista.
% Codominio: Cantidad de choques que se producen en una diagonal inferior.
validate_lower_diagonal(_N,[])->0;
validate_lower_diagonal(N,[H|T])when N-1 =:= H ->1+validate_lower_diagonal(N-1,T);
validate_lower_diagonal(N,[_H|T])->validate_lower_diagonal(N-1,T).

% Dominio: Posición en el tablero y una lista que representa al tablero.
% Codominio: Cantidad de choques totales.
crashes(N,L)->validate_lower_diagonal(N,L)+validate_upper_diagonal(N,L)+validate_row(N,L).

% Dominio: El tamaño del tablero.
% Codominio: Máxima cantidad de choques de un tablero de tamaño N.
board_max_population_fitness(Boardsize) -> (Boardsize * (Boardsize - 1)) / 2.

% Dominio: Una lista.
% Codominio: Choques del tablero.
validar_vec_aux([])->0;
validar_vec_aux([H|T])-> crashes(H,T) + validar_vec_aux(T).
 
% Dominio: Una lista
% Codominio: Valor de la función de aptitud de un tablero de tamaño N.
validar_vec(L)->board_max_population_fitness(length(L))- validar_vec_aux(L).

% Dominio: Una lista de listas.
% Codominio: Una lista con el valor de la función de aptitud de toda la población.
population_fitness([])->[];
population_fitness([H|L])->[validar_vec(H)]++population_fitness(L).
	
% Dominio: Una lista.
% Codominio: Valor de la función de aptitud del mejor individuo.
max_population_fitness([H|T])->max_population_fitness(H,T).
max_population_fitness(I,[])->I;
max_population_fitness(I,[H|T]) when I < H -> max_population_fitness(H,T);
max_population_fitness(I,[_H|T])->max_population_fitness(I,T).
   
% Dominio: Una lista.
% Codominio: La posición del mejor individuo.
best_pos_fitness([H|T])->best_pos_fitness(1,2,H,T).
best_pos_fitness(C,_R,_I,[])->C;
best_pos_fitness(_C,R,I,[H|T]) when I < H -> best_pos_fitness(R,R+1,H,T);
best_pos_fitness(C,R,I,[_H|T])->best_pos_fitness(C,R+1,I,T).

% Dominio: Dos listas.
% Codominio: Una lista.
crossover_aux([H|_T],[_A|B])->  H++B.

% Dominio: Dos listas, la posición para partir las listas.
% Codominio: Una listas.
crossover(L1,L2,I)->crossover_aux(split(L1,I),split(L2,I)).

% Dominio: Una lista, sumatoria de todas las funciones de aptitud.
% Codominio: Una lista de probabilidades.
array_probabilities_aux([],_N)->[];
array_probabilities_aux([H|T],N)-> [validar_vec(H)/N]++array_probabilities_aux(T,N).

% Dominio: Una lista.
% Codominio: Una lista de probabilidades.
array_probabilities(L)->array_probabilities_aux(L,sum(population_fitness(L))).

% Dominio: Una lista de probabilidades.
% Codominio: Una lista de rangos.
selection_ranges([],_N)->[];
selection_ranges([H|T],N)-> [H+N]++selection_ranges(T,N+H).
selection_ranges(L)->selection_ranges(array_probabilities(L),0).

% Dominio: Una lista y lista de rangos.
% Codominio: Una individuo (lista) de forma aleatoria.
select_individual(L,TR)->select_individual(rand:uniform(),TR,L).
select_individual(_N,[],_L)->[];
select_individual(N,[H|_T],[R|_I])when  N =< H -> R;
select_individual(N,[_H|T],[_R|I])->select_individual(N,T,I).

% Dominio: Una lista de listas, una lista, cantidad de la población y lista de rangos.
% Codominio: Una lista de listas.
new_generation(_P,G,0,_TR)->G;
new_generation([],_G,_N,_TR)->[];
new_generation([H|T],G,N,TR)-> new_generation([H|T],G ++ [mutation(individual_exists([H|T],G,crossover(select_individual([H|T],TR),select_individual([H|T],TR),length(H) div 2),TR),rand:uniform())],N-1,TR).
new_generation(P,TR)->new_generation(P,[],length(P)-1,TR).

% Dominio: Una lista de listas, una lista de listas, lista, lista de rangos. 
% Codominio: Una lista.
individual_exists(_P,_G,[],_TR)->[];
individual_exists(_P,[],L,_TR)->L;
individual_exists([C|P],[H|T],L,TR)when L==H-> individual_exists([C|P],[H|T],crossover(select_individual([C|P],TR),select_individual([C|P],TR),length(H) div 2),TR);
individual_exists(P,[_H|T],L,TR)->individual_exists(P,T,L,TR).

% Dominio: Una lista, porcentaje de mutación.
% Codominio: Una lista.
mutation(L,N)when N =< 0.05->insert_at(rand:uniform(length(L)),L,rand:uniform(length(L)));
mutation(L,_N)->L.

% Dominio: Una lista, función de aptitud del individuo apto para la solución del problema, mejor función de aptitud de la generación actual. 
% Codominio: Una lista de listas (solución).
n_queens_aux([],_N,_S)->[];
n_queens_aux(L,N,S)when N==S->lists:nth(best_pos_fitness(population_fitness(L)),L);
n_queens_aux(L,_N,_S)->n_queens([lists:nth(best_pos_fitness(population_fitness(L)),L)]++new_generation(L,selection_ranges(L))).

% Dominio: Una lista de listas.
% Codominio: Una lista de listas.
n_queens([])->[];
n_queens([H|T])->n_queens_aux([H|T],board_max_population_fitness(length(H)),max_population_fitness(population_fitness([H|T]))).

% Dominio: Tamaño del tablero.
% Codominio: Una lista de listas.
geneticosNReinas_aux(N)->n_queens(population_generator(N)).

% Dominio: Tamaño del tablero.
% Codominio: Una lista de listas.
geneticosNReinas(N)->insertar_mtx( create_mtx(N,N), geneticosNReinas_aux(N),1).




