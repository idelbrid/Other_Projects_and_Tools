/**********************************************************************
    Tokenizer for Java source.

    Allows unicode escapes only within strings and comments.  Otherwise,
    accepts all and only those tokens defined by the Java language
    specification.  (Note that this is significantly more than you were
    required to do for assignment 3; this solution would have received a
    lot of extra credit.)

    Tokens are classified as listed in scanner.h.

 **********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "reader.h"
#include "char_classes.h"
#include "keywords.h"
#include "scanner.h"

static void print_location (token_t *tok)
{
    fprintf(stderr, " at line %d, column %d\n",
        tok->location.line->line_num, tok->location.column);
}

/********
    Modify tok to describe next token of input.
    Update loc to refer to location immediately after tok.
 ********/
void scan(location_t * loc, token_t * tok)
{
    enum {
            start,
            done,
            got_fp_dot,
            got_dec,
            got_plus,
            got_minus,
            got_slash,
            got_star,

    } state = start;

/* Standard way to recognize a token: put back lookahead character that
    isn't part of current token: */
#define ACCEPT_REUSE(t) \
    *loc = loc_save;    \
    tok->length--;      \
    tok->tc = t;        \
    state = done;

/* Shortcut to eliminate final states with no out transitions: go
    ahead and accept token in previous state, but don't put back the
    lookahead: it's actually part of the token: */
#define ACCEPT(t) \
    tok->tc = t;  \
    state = done;

    tok->location = *loc;
    tok->length = 0;





    /* ********************** starting the DFA ******************** */
    while (state != done) {
        location_t loc_save = *loc;
        int c = get_character(loc);
        tok->length++;
        switch (state){
        case start :
        	switch (char_classes[c]){

        	case WHITE:
        		break;
        	CASE_DEC_DIGIT:
        		state = got_dec;
        		break;
        	case PLUS:
        		state = got_plus;
        		break;
        	case MINUS:
        		state = got_minus;
        		break;
        	case SLASH:
        		state = got_slash;
        		break;
        	case STAR:
        		state = got_star;
        		break;
        	case DOT:
        		state = got_fp_dot;
        		break;
        	case LPAREN:
        		ACCEPT(T_LPAREN);
        		break;
        	case RPAREN:
        		ACCEPT(T_RPAREN);
        		break;
        	default:
        		fprintf(stderr, "Syntax error: cannot parse symbol ");
        		print_location(tok);  //not an accepted symbol  to start a token
        		ACCEPT_REUSE(T_LITERAL);  //form copied from example
        		break;
        	}
        	break;  //end "start state" switch
        	case got_dec:
        		switch (char_classes[c]){
						CASE_DEC_DIGIT:
							break; //stay put
						case DOT:
							state = got_fp_dot;
							break;
						default:
							ACCEPT_REUSE(T_LITERAL); /* decimal integer */
							break;
        		}
        	break; //end "got_dec" switch
        	case got_plus:
        		switch(char_classes[c]){
						case PLUS:
							fprintf(stderr, "Syntax error: '++' is not implemented yet ");
							print_location(tok);  // ++ not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						case EQUALS:
							fprintf(stderr, "Syntax error: '+=' not implemented yet ");
							print_location(tok);  // += not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						default:
							ACCEPT_REUSE(T_OPERATOR); /* + */
							break;
        		}
        		break;
        	case got_minus:
        		switch(char_classes[c]){
						case MINUS:
							fprintf(stderr, "Syntax error: '--' is not implemented yet ");
							print_location(tok);  // -- not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						case EQUALS:
							fprintf(stderr, "Syntax error: '-=' not implemented yet ");
							print_location(tok);  // -= not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						default:
							ACCEPT_REUSE(T_OPERATOR); /* - */
							break;
				}
				break;
			case got_star:
				switch(char_classes[c]){
						case STAR:
							fprintf(stderr, "Syntax error: '**' is not implemented yet ");
							print_location(tok);  // ** not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						case EQUALS:
							fprintf(stderr, "Syntax error: '*=' not implemented yet ");
							print_location(tok);  // *= not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						default:
							ACCEPT_REUSE(T_OPERATOR); /*  *  */
							break;
				}
				break;
			case got_slash:
				switch(char_classes[c]){
						case SLASH:
							fprintf(stderr, "Syntax error: '//' is not implemented yet ");
							print_location(tok);  // // not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						case EQUALS:
							fprintf(stderr, "Syntax error: '/=' not implemented yet ");
							print_location(tok);  // /= not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						default:
							ACCEPT_REUSE(T_OPERATOR); /*  *  */
							break;
				}
			break;
			case got_fp_dot:
				switch(char_classes[c]){
						CASE_DEC_DIGIT:
							break; //stay put
						case DOT:
							fprintf(stderr, "Syntax error: '..' is not a valid input yet ");
							print_location(tok);  // .. not accepted yet
							ACCEPT_REUSE(T_LITERAL);  //form copied from example
							break;
						default:
							ACCEPT_REUSE(T_LITERAL); /* fp */
							break;
				}
			break;
		default:
			fprintf(stderr, "Went somewhere funky");
			break;
        }

    } //****************** ENDING WHILE LOOP ********************



}
