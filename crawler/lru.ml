(*
    Pantor
    Copyright (C) 2015  Thomas HUET

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

type 'a node = {
  mutable prev : 'a node option;
  value : 'a;
  mutable next : 'a node option;
}

type 'a dlist = {
  mutable head : 'a node option;
  mutable tail : 'a node option;
}

type 'a t = {
  table : ('a, 'a node) Hashtbl.t;
  mutable dlist : 'a dlist;
  mutable vacancy : int;
}

let create n =
  if n < 2 then invalid_arg "create" else
  {
    table = Hashtbl.create n;
    dlist = {
      head = None;
      tail = None;
    };
    vacancy = n;
  }

let mem lru = Hashtbl.mem lru.table

let add lru key =
  if Hashtbl.mem lru.table key then
    let node = Hashtbl.find lru.table key in
    match node.prev with
    | None -> () 
    | Some prev ->
      begin match lru.dlist.tail with
      | None -> ()
      | Some tail ->
        if tail == node then
          lru.dlist.tail <- node.prev
      end;
      prev.next <- node.next;
      begin match node.next with
      | None -> ()
      | Some next ->
        next.prev <- node.prev
      end;
      node.prev <- None;
      node.next <- lru.dlist.head;
      begin match lru.dlist.head with
      | None -> ()
      | Some head ->
        head.prev <- Some node
      end;
      lru.dlist.head <- Some node
  else begin
    if lru.vacancy = 0 then
      match lru.dlist.tail with
      | None -> ()
      | Some tail ->
        Hashtbl.remove lru.table tail.value;
        lru.dlist.tail <- tail.prev;
        match lru.dlist.tail with
        | None -> ()
        | Some tail ->
          tail.next <- None
    else
      lru.vacancy <- lru.vacancy - 1;
    let node = {
      prev = None;
      value = key;
      next = lru.dlist.head;
    } in
    begin match lru.dlist.head with
    | None ->
      lru.dlist.tail <- Some node
    | Some head ->
      head.prev <- Some node
    end;
    lru.dlist.head <- Some node;
    Hashtbl.add lru.table key node
  end
