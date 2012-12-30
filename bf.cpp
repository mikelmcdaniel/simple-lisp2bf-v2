#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <float.h>
#include <vector>

using namespace std;

int * matching_brace_array(const char * bf_str, int bf_str_len) {
	int * matches = new int[bf_str_len];
	vector<int> poss;
	for (int j = 0; j < bf_str_len; j++) {
		switch (bf_str[j]) {
		case '[':
			poss.push_back(j);
			break;
		case ']':
			matches[j] = poss.back();
			matches[poss.back()] = j;
			poss.pop_back();
			break;
		}
	}
	return matches;
}

int eval_bf(const char * bf_str, int * stack=NULL, int index=0, 
		int bf_index=0, FILE * in_file=stdin, FILE * out_file=stdout) {
	int bf_str_len = strlen(bf_str);
	int * matching_braces = matching_brace_array(bf_str, bf_str_len);

	for (int j = bf_index; j < bf_str_len; j++) {
		switch (bf_str[j]) {
		case '>':
			index++;
			break;
		case '<':
			index--;
			break;
		case '+':
			stack[index]++;
			stack[index] %= 256;
			break;
		case '-':
			stack[index]--;
			stack[index] %= 256;
			break;
		case ']':
			if (stack[index])
				j = matching_braces[j];
			break;
		case '[':
			if (!stack[index])
				j = matching_braces[j];
			break;
		case '.':
			fputc(stack[index], out_file);
			break;
		case ',':
			stack[index] = fgetc(in_file);
			break;
		}
	}
	
	delete[] matching_braces;
	return index;
}

int eval_bf_safe(const char * bf_str, int * stack=NULL, int index=0, 
		int max_steps=1000, int stack_size=30000,
		int bf_index=0, FILE * in_file=stdin, FILE * out_file=stdout) {
	int bf_str_len = strlen(bf_str);
	int * matching_braces = matching_brace_array(bf_str, bf_str_len);

	for (int j = bf_index; j < bf_str_len; j++) {
		if (!max_steps) return -1;
		max_steps--;
		switch (bf_str[j]) {
		case '>':
			index++;
			if (index == stack_size) return -2;
			break;
		case '<':
			index--;
			if (index == -1) return -3;
			break;
		case '+':
			stack[index]++;
			stack[index] %= 256;
			break;
		case '-':
			stack[index]--;
			stack[index] %= 256;
			break;
		case ']':
			if (stack[index])
				j = matching_braces[j];
			break;
		case '[':
			if (!stack[index])
				j = matching_braces[j];
			break;
		case '.':
			fputc(stack[index], out_file);
			break;
		case ',':
			stack[index] = fgetc(in_file);
			break;
		}
	}
	
	delete[] matching_braces;
	return index;
}

void str_insert(char * str, int str_len, int pos, char item) {
	memmove(&str[pos + 1], &str[pos], str_len - pos + 1);
	str[pos] = item;
}

void str_delete(char * str, int str_len, int pos) {
	memmove(&str[pos], &str[pos + 1], str_len - pos);
}

char bf_char(int x) {
	return "<>+-[].,"[x];
}

// NOTE: be careful using this function; in the worst case, it will add
// 2*num_mutations bytes to bf_str and cannot check for buffer overflow
int mutate(char * bf_str, int bf_str_len, int max_len=20, int num_mutations=1) {
	int pos;
	char c;
	while (num_mutations--) {
		switch (rand() % 2) {
		case 0: // insert
			if (bf_str_len >= max_len - 1) break;
			c = bf_char(rand() % 5);
			pos = rand() % (bf_str_len + 1);
			str_insert(bf_str, bf_str_len, pos, c);
			bf_str_len++;
			if (c == '[') {
				// TODO: check for off-by-one error
				pos = pos + 1 + (rand() % (bf_str_len - pos));
				str_insert(bf_str, bf_str_len, pos, ']');
				bf_str_len++;
			}
			break;
		case 1: // delete
		case 2: // delete
			if (bf_str_len == 0) break;
			pos = rand() % bf_str_len;
			c = bf_str[pos];
			str_delete(bf_str, bf_str_len, pos);
			bf_str_len--;
			if (c == '[') {
				while (bf_str[pos] != ']') pos++;
				str_delete(bf_str, bf_str_len, pos);
				bf_str_len--;
			} else if (c == ']') {
				pos--;
				while (bf_str[pos] != '[') pos--;
				str_delete(bf_str, bf_str_len, pos);
				bf_str_len--;
			}
			break;
		}
	}
	
	return bf_str_len;
}

double stack_score(int * stack, int * desired_stack, int stack_size) {
	double score = 0;
	for (int j = 0; j < stack_size; j++) {
		int temp = stack[j] - desired_stack[j];
		//if (temp < 0) temp = -temp;
		score += temp * temp;
		//if (score < 0)
		//	return (INT_MAX >> 1);
	}
	return score;
}

void print_stack(int * stack, int stack_size) {
	printf("[");
	if (stack_size >= 0)
		printf("%d", stack[0] % 256);
	for (int j = 1; j < stack_size; j++) {
		printf(" %d", stack[j] % 256);
	}
	printf("]\n");
}

void foo() {
	int num_children = 200;
	int buf_len = 400;
	int stack_size = 20;
	int max_iters = 400000;
	int mutation_rate = 1;

	char ** offspring = new char*[num_children];
	int * offspring_len = new int[num_children];
	// init
	for (int j = 0; j < num_children; j++) {
		offspring[j] = new char[buf_len];
		offspring[j][0] = '\0'; // make them proper strings
		offspring_len[j] = 0;
	}
	
	int * stack = new int[stack_size];
	int * desired_stack = new int[stack_size];
	memset(stack, 0, stack_size * sizeof(stack[0]));
	
	int a[50], b[50];
	
	for (int j = 0; j < 50; j++) {
		a[j] = rand() % 256;
		b[j] = rand() % 256;
	}
	
	double best_score = DBL_MAX / 2.0;
	for (int iter = 0; iter < max_iters; iter++) {
		//int best_score = INT_MAX >> 1;
		int best_j = 0;
		
		
		for (int j = 0; j < num_children; j++) {
			double cur_score = 0;
			//int cur_score = 0;
			for (int k = 0; k < 50; k++) {
				memset(desired_stack, 0, stack_size * sizeof(desired_stack[0]));
				desired_stack[0] = ((unsigned int)a[k] ^ (unsigned int)b[k]) % 256;
				memset(stack, 0, stack_size);
				stack[0] = a[k];
				stack[1] = b[k];
				int result = eval_bf_safe(offspring[j], stack, 2, 256*256*256, stack_size);
				if (result < 0) {// if there was an error (like running off the tape)
					cur_score = best_score + 1;
					break;
				}
				//cur_score += stack_score(stack, desired_stack, stack_size);
				cur_score += stack_score(stack, desired_stack, 1);
			}
			if (cur_score < best_score)
				mutation_rate = 1;
			if (cur_score <= best_score) {
				best_score = cur_score;
				best_j = j;
				if (cur_score == 0) {
					puts("YES!");
					print_stack(stack, stack_size);
					print_stack(desired_stack, stack_size);
					puts(offspring[j]);
					//exit(0);
					scanf("%*c");
				}
			}
		}
		
		
		// copy the best into the first slot
		strcpy(offspring[0], offspring[best_j]);
		offspring_len[0] = offspring_len[best_j];
		// copy the best(first) into all other slots, then mutate
		for (int j = 0; j < num_children; j++) {
			strcpy(offspring[j], offspring[0]);
			offspring_len[j] = mutate(offspring[j], offspring_len[0], buf_len, (j + 1) * mutation_rate / num_children);
		}

		mutation_rate++;
		
		/*
		for (int j = 0; j < num_children; j++) {
			offspring_len[j] = mutate(offspring[j], offspring_len[j], buf_len, 1 + j % 5);
		}
		*/
		
		//puts(offspring[0]);
		printf("%f(%d)\t%s\n", best_score, mutation_rate, offspring[0]);
	}
	
	// cleanup
	delete[] desired_stack;
	delete[] stack;
	for (int j = 0; j < num_children; j++) {
		delete[] offspring[j];
	}
	delete[] offspring;
	delete[] offspring_len;
}

// This function doesn't check return values of fseek, ftell, fread, etc.
char * file_contents(const char * file_name, int * contents_size=NULL) {
	FILE * fin = fopen(file_name, "r");
	int file_size;
	char * buffer;
	
	fseek(fin, 0, SEEK_END);
	file_size = ftell(fin);
	fseek(fin, 0, SEEK_SET);
	buffer = new char[file_size + 1];
	fread(buffer, 1, file_size, fin);
	buffer[file_size] = '\0';
	fclose(fin);
	
	if (contents_size)
		*contents_size = file_size + 1;
	return buffer;
}

void eval_bf_file(const char * file_name) {
	char * bf_str = file_contents(file_name);
	int * stack = new int[30000];
	memset(stack, 0, 30000 * sizeof(stack[0]));
	eval_bf(bf_str, stack);
	delete[] stack;
	delete[] bf_str;
}

int main(int argc, const char ** argv) {
	//srand(16);
	//srand(time(NULL));
	eval_bf_file(argv[1]);
	//foo();
	return 0;
}
