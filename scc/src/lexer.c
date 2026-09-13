#ifndef LEXER_H
#define LEXER_H

#include <stdbool.h>

typedef enum
{
	TOK_LEFT_PAREN,
	TOK_RIGHT_PAREN,
	TOK_LEFT_BRACKET,
	TOK_RIGHT_BRACKET,
	TOK_LEFT_BRACE,
	TOK_RIGHT_BRACE,
	TOK_COMMA,
	TOK_SEMICOLON,
	TOK_DOT,
	TOK_DOT_STAR,
	TOK_COLON,
	TOK_EQUAL,
	TOK_ARROW,
	TOK_AMPERSAND,
	TOK_DECREMENT,
	TOK_INCREMENT,
	TOK_TILDE,
	TOK_EXCLAMATION,
	TOK_STAR,
	TOK_PLUS,
	TOK_MINUS,
	TOK_SLASH,
	TOK_PERCENT,
	TOK_EQ,
	TOK_NEQ,
	TOK_LT,
	TOK_LE,
	TOK_GT,
	TOK_GE,

	TOK_IDENTIFIER,
	TOK_BUILTIN,
	TOK_STRING,
	TOK_NUMBER,

	TOK_FUN,
	TOK_TRUE,
	TOK_FALSE,
	TOK_IF,
	TOK_THEN,
	TOK_ELSE,
	TOK_FOR,
	TOK_DO,
	TOK_RETURN,
	TOK_BREAK,
	TOK_CONTINUE,
	TOK_VAL,
	TOK_VAR,
	TOK_TYPE,
	TOK_STRUCT,
	TOK_UNION,
	TOK_AND,
	TOK_OR,
	TOK_AS,

	TOK_EOF,
} TokenKind;

typedef struct
{
	TokenKind kind;
	char* start;
	size len;
	int line;

	// for floating-point constants
	bool hasDot;
} Token;

typedef struct
{
	const char* start;
	char* current;
	int line;
	Token peekedToken;
	bool hasPeeked;
} Lexer;

void initLexer(const char* source);
Token nextToken(void);
Token peekToken(void);

#endif
#if __INCLUDE_LEVEL__ == 0

#include "log.c"
#include "string.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

Lexer lexer;

void initLexer(const char* source)
{
	lexer.start = source;
	lexer.current = (char*)source;
	lexer.line = 0;
}

bool isAtEnd(void) { return *lexer.current == '\0'; }

Token makeToken(TokenKind t)
{
	Token token;
	token.kind = t;
	token.start = (char*)lexer.start;
	token.len = (size)(lexer.current - lexer.start);
	token.line = lexer.line;
	token.hasDot = false;
	return token;
}

Token makeConstantToken(TokenKind t, bool hasDot)
{
	Token tok;
	tok = makeToken(t);
	tok.hasDot = hasDot;
	return tok;
}

char advance(void)
{
	lexer.current++;
	return lexer.current[-1];
}

char peek(void) { return *lexer.current; }

bool isNext(char expected)
{
	char c = advance();
	if (c == expected)
		return true;
	return false;
}

void skipWhitespace(void)
{
	while (true)
	{
		char n = peek();
		switch (n)
		{
		case '\n':
			lexer.line++;
		case ' ':
		case '\t':
		case '\r':
			advance();
			break;
		case '#':
			advance();
			if (isNext('#'))
			{
				int delimLength = 2;
				int tryEndLength = 0;
				while (isNext('#'))
					delimLength++;
				while (tryEndLength != delimLength)
				{
					while (!isNext('#') && !isAtEnd())
						;
					if (isAtEnd())
						logFatal("Unterminated block comment.");
					tryEndLength = 1;
					while (isNext('#'))
						tryEndLength++;
				}
			}
			else
			{
				while (peek() != '\n' && !isAtEnd())
					advance();
			}
			break;
		default:
			return;
		}
	}
}

Token lexString(void)
{
	while (peek() != '"' && !isAtEnd())
	{
		if (peek() == '\n')
			lexer.line++;
		advance();
	}
	if (isAtEnd())
		logFatal("String is not terminated.");
	advance();
	return makeToken(TOK_STRING);
}

bool isDigit(char c)
{
	switch (c)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'a':
	case 'A':
	case 'b':
	case 'B':
	case 'c':
	case 'C':
	case 'd':
	case 'D':
	case 'e':
	case 'E':
	case 'f':
	case 'F':
	case 'x':
	case 'X':
	case 'o':
	case 'O':
		return true;
	default:
		return false;
	}
}

bool isLetter(char c) { return isalpha(c) || c == '_'; }

// in the lexer a lot more is allowed
// the number validation happens in the parser
Token lexNumber(void)
{
	bool hadDot = false;
	while (isDigit(peek()))
		advance();
	if (peek() == '.')
	{
		hadDot = true;
		advance();
		if (isDigit(peek()))
		{
			while (isDigit(peek()))
				advance();
		}
		else
		{
			logError("An integer literal has no members to possibly access.");
		}
	}
	char p = peek();
	if (p == 'i' || p == 'u' || p == 'f')
	{
		advance();
		if (peek() != 8)
			advance();
		advance();
	}
	return makeConstantToken(TOK_NUMBER, hadDot);
}

TokenKind checkKeyword(const char* key, long keyLen, TokenKind k)
{
	if ((keyLen == lexer.current - lexer.start) && (memcmp(lexer.start, key, (unsigned)keyLen) == 0))
	{
		return k;
	}
	else
	{
		return TOK_IDENTIFIER;
	}
}

TokenKind classifyIdent(void)
{
	switch (lexer.start[0])
	{
	case 'a':
		switch (lexer.start[1])
		{
		case 'n':
			return checkKeyword("and", 3, TOK_AND);
		case 's':
			return checkKeyword("as", 2, TOK_AS);
		}
		break;
	case 'b':
		return checkKeyword("break", 5, TOK_BREAK);
	case 'c':
		return checkKeyword("continue", 8, TOK_CONTINUE);
	case 'd':
		return checkKeyword("do", 2, TOK_DO);
	case 'e':
		return checkKeyword("else", 4, TOK_ELSE);
	case 'f':
		switch (lexer.start[1])
		{
		case 'u':
			return checkKeyword("fun", 3, TOK_FUN);
		case 'o':
			return checkKeyword("for", 3, TOK_FOR);
		case 'a':
			return checkKeyword("false", 5, TOK_FALSE);
		}
		break;
	case 'i':
		return checkKeyword("if", 2, TOK_IF);
	case 'o':
		return checkKeyword("or", 2, TOK_OR);
	case 'r':
		return checkKeyword("return", 6, TOK_RETURN);
	case 's':
		return checkKeyword("struct", 6, TOK_STRUCT);
	case 't':
		switch (lexer.start[1])
		{
		case 'h':
			return checkKeyword("then", 4, TOK_THEN);
		case 'y':
			return checkKeyword("type", 4, TOK_TYPE);
		case 'r':
			return checkKeyword("true", 4, TOK_TRUE);
		}
		break;
	case 'u':
		return checkKeyword("union", 5, TOK_UNION);
	case 'v':
		switch (lexer.start[2])
		{
		case 'l':
			return checkKeyword("val", 3, TOK_VAL);
		case 'r':
			return checkKeyword("var", 3, TOK_VAR);
		}
		break;
	}

	return TOK_IDENTIFIER;
}

Token lexIdentifier(void)
{
	char p = peek();
	while (isLetter(p) || isDigit(p))
	{
		advance();
		p = peek();
	}
	return makeToken(classifyIdent());
}

Token lexBuiltin(void)
{
	char p = peek();
	while (isLetter(p) || isDigit(p))
	{
		advance();
		p = peek();
	}
	return makeToken(TOK_BUILTIN);
}

Token peekToken(void)
{
	if (lexer.hasPeeked)
	{
		return lexer.peekedToken;
	}
	lexer.peekedToken = nextToken();
	lexer.hasPeeked = true;
	return lexer.peekedToken;
}

Token nextToken(void)
{
	if (lexer.hasPeeked)
	{
		lexer.hasPeeked = false;
		return lexer.peekedToken;
	}
	skipWhitespace();
	lexer.start = lexer.current;

	if (isAtEnd())
	{
		return makeToken(TOK_EOF);
	}
	char next = advance();
	if (isLetter(next))
		return lexIdentifier();
	if (isDigit(next))
		return lexNumber();

	switch (next)
	{
	case '@':
		return lexBuiltin();
	case '(':
		return makeToken(TOK_LEFT_PAREN);
	case ')':
		return makeToken(TOK_RIGHT_PAREN);
	case '[':
		return makeToken(TOK_LEFT_BRACKET);
	case ']':
		return makeToken(TOK_RIGHT_BRACKET);
	case '{':
		return makeToken(TOK_LEFT_BRACE);
	case '}':
		return makeToken(TOK_RIGHT_BRACE);
	case ',':
		return makeToken(TOK_COMMA);
	case ';':
		return makeToken(TOK_SEMICOLON);
	case '.':
		if (peek() == '*')
		{
			advance();
			return makeToken(TOK_DOT_STAR);
		}
		return makeToken(TOK_DOT);
	case ':':
		return makeToken(TOK_COLON);
	case '=':
		if (peek() == '=')
		{
			advance();
			return makeToken(TOK_EQ);
		}
		return makeToken(TOK_EQUAL);
	case '<':
		if (peek() == '=')
		{
			advance();
			return makeToken(TOK_LE);
		}
		return makeToken(TOK_LT);
	case '>':
		if (peek() == '=')
		{
			advance();
			return makeToken(TOK_GE);
		}
		return makeToken(TOK_GT);
	case '-':
		if (peek() == '>')
		{
			advance();
			return makeToken(TOK_ARROW);
		}
		else if (peek() == '-')
		{
			advance();
			return makeToken(TOK_DECREMENT);
		}
		else
		{
			return makeToken(TOK_MINUS);
		}
	case '~':
		return makeToken(TOK_TILDE);
	case '!':
		if (peek() == '=')
		{
			advance();
			return makeToken(TOK_NEQ);
		}
		return makeToken(TOK_EXCLAMATION);
	case '&':
		return makeToken(TOK_AMPERSAND);
	case '*':
		return makeToken(TOK_STAR);
	case '/':
		return makeToken(TOK_SLASH);
	case '%':
		return makeToken(TOK_PERCENT);
	case '+':
		if (peek() == '+')
		{
			advance();
			return makeToken(TOK_INCREMENT);
		}
		else
		{
			return makeToken(TOK_PLUS);
		}
	case '"':
		return lexString();
	}

	logFatal("Input '%c' can not be tokenized.", next);
}

#endif
