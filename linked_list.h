#include<stdio.h> 
#include<malloc.h>

#ifndef LINKED_LIST   /* Include guard */
#define LINKER_LIST


typedef struct node{
    void * valp;
    struct node * next;
}node;


typedef void (*traverse_fun)(void * p);

void foreach(node * head, traverse_fun fun);

/*add an element to the head of the list*/
node * push(node * head, void * valp);

/*
function : remove the first element of list, 
           2-level pointer since the head address is changed
args : the pointer to the address of the head of list
return : the valp of the first element

void * pop(node ** head) {
    void * retval = NULL;
    node * next_node = NULL;

    if (*head == NULL) {
        return NULL;
    }

    next_node = (*head)->next;
    retval = (*head)->valp;
    free(*head);
    *head = next_node;

    return retval;
}*/


/*
function : remove the n-th element of list
return : the valp of the n-th element
*/
void * remove_by_index(node ** head, int n);

node * init_list();

void destroy_list(node * head, traverse_fun free_val);

#endif 

