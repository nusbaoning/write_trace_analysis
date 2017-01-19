#include<stdio.h>
#include<malloc.h>
#include"linked_list.h"

void foreach(node * head, traverse_fun fun) {
	
    node * current = head;
	printf("debug : print a list \n");
    while (current != NULL) {
        fun(current);
        current = current->next;
    }
    
}

node * push(node * head, void * valp) {
	node * current;
	// 0 element
	if(head -> valp == NULL) {
		head -> valp = valp;
		printf("debug : head next = %p\n", head->next);
		return head;
	}	
	// int a;
	//printf("1");
	//scanf("%d",&a);
	current = (node *)malloc(sizeof(node));   
	//printf("2");
	//scanf("%d", &a);
	
	current->valp = valp;  
	current->next = head;
	return current;
	
}

void * remove_by_index(node ** head, int n) {
	printf("debug : 1.0 enter remove\n");
    int i = 0;
    void * retval = NULL;
    node * current = *head;
    node * temp_node = NULL;
	
    if (n < 0 || current==NULL || current->valp==NULL) {
        return NULL;
    }

    if(n == 0) {    	
    	retval = current -> valp;
    	temp_node = current->next;
    	if(temp_node == NULL)
    		return retval;
	}
       
	else {
	
    	for (i = 0; i < n-1; i++) {
        	if (current->next == NULL) {
            	return NULL;
        	}
        	current = current->next;
    	}    	
    	temp_node = current->next;
    	retval = temp_node->valp;
	}
	
    current->next = temp_node->next;
    free(temp_node);
	return retval;
}

node * init_list(){
	node * head = (node *)malloc(sizeof(node));
	printf("debug : %p\n", head);
    if (head == NULL)
    {
        perror("Error printed by perror");
        return NULL;
    }
    head -> valp = NULL;
    head -> next = NULL;
    return head;
}

void destroy_list(node * head, traverse_fun free_val) {
    node* current = head;
    node* temp;
    // int a;
    while(current!=NULL){
        temp=current->next;
		free_val(current->valp);
        free(current);
        current=temp;
    }
    head=NULL;
}
