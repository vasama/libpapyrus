#include "Eval.h"

#include "Arena.h"
#include "Macros.h"
#include "Script.h"

#include <assert.h>
#include <string.h>

static struct Eval
String_Add(struct Eval lhs, struct Eval rhs, struct Arena* arena)
{
	struct Eval result;
	result.type = Eval_String;
	result.string_size = lhs.string_size + rhs.string_size;
	if (lhs.string_data + lhs.string_size == rhs.string_data)
	{
		result.string_data = lhs.string_data;
	}
	else
	{
		char* buffer = Arena_Allocate(arena, result.string_size);
		memcpy(buffer, lhs.string_data, lhs.string_size);
		memcpy(buffer + lhs.string_size, rhs.string_data, rhs.string_data);
		result.string_data = buffer;
	}
	return result;
}

static struct Eval
String_Eq(struct Eval lhs, struct Eval rhs)
{
	return (struct Eval) {
		.type = Eval_Bool,
		.bool_ = memcmp(lhs.string_data, rhs.string_data,
			MIN(lhs.string_size, rhs.string_size)) == 0,
	};
}

static struct Eval
String_Ne(struct Eval lhs, struct Eval rhs)
{
	return (struct Eval) {
		.type = Eval_Bool,
		.bool_ = memcmp(lhs.string_data, rhs.string_data,
			MIN(lhs.string_size, rhs.string_size)) != 0,
	};
}

static struct Eval
String_Lt(struct Eval lhs, struct Eval rhs)
{
	return (struct Eval) {
		.type = Eval_Bool,
		.bool_ = memcmp(lhs.string_data, rhs.string_data,
			MIN(lhs.string_size, rhs.string_size)) < 0,
	};
}

static struct Eval
String_Gt(struct Eval lhs, struct Eval rhs)
{
	return (struct Eval) {
		.type = Eval_Bool,
		.bool_ = memcmp(lhs.string_data, rhs.string_data,
			MIN(lhs.string_size, rhs.string_size)) > 0,
	};
}

static struct Eval
String_Le(struct Eval lhs, struct Eval rhs)
{
	return (struct Eval) {
		.type = Eval_Bool,
		.bool_ = memcmp(lhs.string_data, rhs.string_data,
			MIN(lhs.string_size, rhs.string_size)) <= 0,
	};
}

static struct Eval
String_Ge(struct Eval lhs, struct Eval rhs)
{
	return (struct Eval) {
		.type = Eval_Bool,
		.bool_ = memcmp(lhs.string_data, rhs.string_data,
			MIN(lhs.string_size, rhs.string_size)) >= 0,
	};
}

struct Eval
Papyrus_Eval(const struct Papyrus_Expr* expr, struct Arena* arena)
{
	assert(expr->flags & Papyrus_ExprFlags_Const);

	struct Eval result;
	switch (expr->kind)
	{
#define CONSTANT(t, v) \
	case Papyrus_Expr_Lit ## t: \
		result.type = Eval_ ## t; \
		result.v = expr->lit.v; \
		break

		CONSTANT(Int, int_);
		CONSTANT(Bool, bool_);
		CONSTANT(Float, float_);
#undef CONSTANT

	case Papyrus_Expr_LitString:
		result.type = Eval_String;
		result.string_data = expr->lit.string.data;
		result.string_size = (uint32_t)expr->lit.string.size;
		break;

	case Papyrus_Expr_Neg:
		{
			struct Eval sub = Papyrus_Eval(expr, arena);
			result.type = sub.type;
			switch (sub.type)
			{
			case Eval_Int:
				result.int_ = -sub.int_;
				break;

			case Eval_Float:
				result.float_ = -sub.float_;
				break;
			}
		}
		break;

	case Papyrus_Expr_Not:
		{
			struct Eval sub = Papyrus_Eval(expr, arena);
			result.type = Eval_Bool;
			switch (sub.type)
			{
			case Eval_Int:
				sub.bool_ = sub.int_ == 0;
				break;

			case Eval_Bool:
				sub.bool_ = !sub.bool_;
				break;

			case Eval_Float:
				sub.bool_ = sub.float_ == 0.0;
				break;

			case Eval_String:
				sub.bool_ = sub.string_size == 0;
				break;
			}
		}
		break;

#define BINARY(x, ...) \
	case Papyrus_Expr_ ## x: \
		{ \
			struct Eval lhs = Papyrus_Eval(expr, arena); \
			struct Eval rhs = Papyrus_Eval(expr, arena); \
			switch (lhs.type) { __VA_ARGS__ } \
		} \
		break

#define OPERATOR(t, v, o) \
	case Eval_ ## t: \
		result.type = lhs.type; \
		result.v = lhs.v o rhs.v; \
		break;

#define FUNCTION(t, f) \
	case Eval_ ## t: \
		result = f(lhs, rhs, arena); \
		break;

		BINARY(Add,
			OPERATOR(Int, int_, +)
			OPERATOR(Float, float_, +)
			FUNCTION(String, String_Add));
		
		BINARY(Sub,
			OPERATOR(Int, int_, -)
			OPERATOR(Float, float_, -));

		BINARY(Mul,
			OPERATOR(Int, int_, *)
			OPERATOR(Float, float_, *));

		BINARY(Div,
			OPERATOR(Int, int_, /)
			OPERATOR(Float, float_, /));

		BINARY(Mod,
			OPERATOR(Int, int_, %));

		BINARY(Con,
			OPERATOR(Bool, bool_, &&));

		BINARY(Dis,
			OPERATOR(Bool, bool_, ||));

#undef FUNCTION
#undef OPERATOR
#undef BINARY

#define COMPARISON(x, ...) \
	case Papyrus_Expr_ ## x: \
		{ \
			struct Eval lhs = Papyrus_Eval(expr, arena); \
			struct Eval rhs = Papyrus_Eval(expr, arena); \
			switch (lhs.type) { __VA_ARGS__ } \
		} \
		break

#define OPERATOR(t, v, o) \
	case Eval_ ## t: \
		result.type = Eval_Bool; \
		result.bool_ = lhs.v o rhs.v; \
		break;

#define FUNCTION(t, f) \
	case Eval_ ## t: \
		result = f(lhs, rhs); \
		break;

		COMPARISON(Eq,
			OPERATOR(Int, int_, ==)
			OPERATOR(Bool, bool_, ==)
			OPERATOR(Float, float_, ==)
			FUNCTION(String, String_Eq));
		
		COMPARISON(Ne,
			OPERATOR(Int, int_, !=)
			OPERATOR(Bool, bool_, !=)
			OPERATOR(Float, float_, !=)
			FUNCTION(String, String_Ne));

		COMPARISON(Lt,
			OPERATOR(Int, int_, <)
			OPERATOR(Float, float_, <)
			FUNCTION(String, String_Lt));

		COMPARISON(Gt,
			OPERATOR(Int, int_, >)
			OPERATOR(Float, float_, >)
			FUNCTION(String, String_Gt));
		
		COMPARISON(Le,
			OPERATOR(Int, int_, <=)
			OPERATOR(Float, float_, <=)
			FUNCTION(String, String_Le));

		COMPARISON(Ge,
			OPERATOR(Int, int_, >=)
			OPERATOR(Float, float_, >=)
			FUNCTION(String, String_Ge));

#undef FUNCTION
#undef OPERATOR
#undef COMPARISON

	case Papyrus_Expr_Cast:
		{
			struct Eval sub = Papyrus_Eval(expr->cast.expr, arena);
			switch (expr->cast.type->type)
			{
			case Papyrus_Type_Int:
				switch (sub.type)
				{
				case Eval_Int:
					result = sub;
					break;

				case Eval_Float:
					result.int_ = (int32_t)sub.float_;
					break;

				case Eval_String:
					{
						assert(false);
					}
					break;
				}
				result.type = Eval_Int;
				break;

			case Papyrus_Type_Bool:
				switch (sub.type)
				{
				case Eval_Int:
					sub.bool_ = sub.int_ != 0;
					break;

				case Eval_Bool:
					sub.bool_ = sub.bool_;
					break;

				case Eval_Float:
					sub.bool_ = sub.float_ != 0.0;
					break;

				case Eval_String:
					sub.bool_ = sub.string_size != 0;
					break;
				}
				result.type = Eval_Bool;
				break;

			case Papyrus_Type_Float:
				switch (sub.type)
				{
				case Eval_Int:
					result.float_ = (float)sub.int_;
					break;

				case Eval_Float:
					result = sub;
					break;

				case Eval_String:
					{
						assert(false);
					}
					break;
				}
				result.type = Eval_Int;
				break;

			case Papyrus_Type_String:
				assert(false);
				break;
			}
		}
		break;
	}
}
