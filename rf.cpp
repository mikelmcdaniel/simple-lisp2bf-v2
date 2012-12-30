#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <vector>

using namespace std;

int * matching_brace_array(const char * rf_str, int rf_str_len) {
	int * matches = new int[rf_str_len];
	vector<int> poss;
	for (int j = 0; j < rf_str_len; j++) {
		switch (rf_str[j]) {
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

int eval_rf(const char * rf_str, char * stack=NULL, int index=0, 
		int rf_index=0, FILE * in_file=stdin, FILE * out_file=stdout) {
	int rf_str_len = strlen(rf_str);
	int * matching_braces = matching_brace_array(rf_str, rf_str_len);

	for (int j = rf_index; j < rf_str_len; j++) {
		switch (rf_str[j]) {
		case '>':
			index++;
			break;
		case '<':
			index--;
			break;
		case '+':
			stack[index]++;
			break;
		case '-':
			stack[index]--;
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

int eval_rf_safe(const char * rf_str, char * stack=NULL, int index=0, 
		int max_steps=1000, int stack_size=30000,
		int rf_index=0, FILE * in_file=stdin, FILE * out_file=stdout) {
	int rf_str_len = strlen(rf_str);
	int * matching_braces = matching_brace_array(rf_str, rf_str_len);
	int * indicies = new int[stack_size * 2];
	int num_indicies = 0;
	int steps = max_steps;

	for (int j = rf_index; j < rf_str_len; j++) {
		if (!steps) {
			index = -1;
			break;
		}
		steps--;
		switch (rf_str[j]) {
		case '+':
			stack[index]++;
			break;
		case '-':
			stack[index]--;
			break;
		case ']':
			if (stack[index]) {
				j = matching_braces[j];
			} else {
				num_indicies--;
			}
			break;
		case '[':
			if (!stack[index]) {
				j = matching_braces[j];
			} else {
				indicies[num_indicies++] = index;
				steps = max_steps;
				max_steps--;
			}
			break;
		case '.':
			fputc(stack[index], out_file);
			break;
		case ',':
			stack[index] = fgetc(in_file);
			break;
		default:
			if (rf_str[j] >= 'a' && rf_str[j] <= 'z') {
				index = rf_str[j] - 'a';
			}
			break;
		}
	}
	
	delete[] indicies;
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

char rf_char(int x) {
	return "+-[].,"[x];
}

// NOTE: be careful using this function; in the worst case, it will add
// 2*num_mutations bytes to rf_str and cannot check for buffer overflow
int mutate(char * rf_str, int rf_str_len, int num_registers, int max_len=20, int num_mutations=1) {
	int pos;
	char c;
	while (num_mutations--) {
		switch (rand() % 4) {
		case 0: // insert bf char
			if (rf_str_len >= max_len - 1) break;
			c = "+-["[rand() % 3];
			pos = rand() % (rf_str_len + 1);
			str_insert(rf_str, rf_str_len, pos, c);
			rf_str_len++;
			if (c == '[') {
				pos = pos + 1 + (rand() % (rf_str_len - pos));
				str_insert(rf_str, rf_str_len, pos, ']');
				rf_str_len++;
			}
			break;
		case 1: // insert register char
			if (rf_str_len >= max_len - 1) break;
			c = 'a' + rand() % num_registers;
			pos = rand() % (rf_str_len + 1);
			str_insert(rf_str, rf_str_len, pos, c);
			rf_str_len++;
			break;
		case 2: // delete
		case 3: // delete
		case 4: // delete
			if (rf_str_len == 0) break;
			pos = rand() % rf_str_len;
			c = rf_str[pos];
			str_delete(rf_str, rf_str_len, pos);
			rf_str_len--;
			if (c == '[') {
				while (rf_str[pos] != ']') pos++;
				str_delete(rf_str, rf_str_len, pos);
				rf_str_len--;
			} else if (c == ']') {
				pos--;
				while (rf_str[pos] != '[') pos--;
				str_delete(rf_str, rf_str_len, pos);
				rf_str_len--;
			}
			break;
		}
	}
	
	return rf_str_len;
}

int stack_score(char * stack, char * desired_stack, int stack_size) {
	int score = 0;
	for (int j = 0; j < stack_size; j++) {
		int temp = stack[j] - desired_stack[j];
		if (temp < 0) temp = -temp;
		score += temp * temp;
		if (score < 0)
			return (INT_MAX >> 1);
	}
	return score;
}

void print_stack(char * stack, int stack_size) {
	printf("[");
	if (stack_size >= 0)
		printf("%d", stack[0] % 256);
	for (int j = 1; j < stack_size; j++) {
		printf(" %d", stack[j] % 256);
	}
	printf("]\n");
}

void foo() {
	int num_children = 20;
	int buf_len = 400;
	int stack_size = 20;
	int max_iters = 400000;
	

	char ** offspring = new char*[num_children];
	int * offspring_len = new int[num_children];
	// init
	for (int j = 0; j < num_children; j++) {
		offspring[j] = new char[buf_len];
		
		strcpy(offspring[j], "");
		offspring_len[j] = strlen(offspring[j]);
	}
	
	char * stack = new char[stack_size];
	char * desired_stack = new char[stack_size];
	memset(stack, 0, stack_size);
	
	char a[50], b[50];
	
	for (int j = 0; j < 50; j++) {
		a[j] = rand() % 256;
		b[j] = rand() % 256;
	}

	for (int iter = 0; iter < max_iters; iter++) {
		int best_score = (INT_MAX >> 1), best_j = 0;
		
		for (int j = 0; j < num_children; j++) {
			int cur_score = 0;
			
			for (int k = 0; k < 50; k++) {
				memset(desired_stack, 0, stack_size);
				desired_stack[0] = ((unsigned int)a[k] * (unsigned int)b[k]) % 256;
				memset(stack, 0, stack_size);
				stack[0] = a[k];
				stack[1] = b[k];
				int result = eval_rf_safe(offspring[j], stack, 2, 256*2, stack_size);
				if (result < 0) {// if there was an error (like running off the tape)
					cur_score = best_score + 1;
					break;
				}
				//cur_score += stack_score(stack, desired_stack, stack_size);
				cur_score += (stack[0] - desired_stack[0]) * (stack[0] - desired_stack[0]);
			}
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
		for (int j = 1; j < num_children; j++) {
			strcpy(offspring[j], offspring[0]);
			offspring_len[j] = mutate(offspring[j], offspring_len[0], stack_size, buf_len, 1 + j);
		}
		
		if (iter % 30 == 0)
			printf("%d\t%s\n", best_score, offspring[0]);
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

void eval_rf_file(const char * file_name) {
	char * rf_str = file_contents(file_name);
	char * stack = new char[30000];
	memset(stack, 0, sizeof(stack));
	eval_rf(rf_str, stack);
	delete[] stack;
	delete[] rf_str;
}

int main(int argc, const char ** argv) {
	//srand(16);
	srand(time(NULL));
	//eval_rf_file(argv[1]);
	foo();
	return 0;
}
