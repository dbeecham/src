#include <stdio.h>
#include <z3.h>


struct signal_pattern {
    Z3_ast a;
    Z3_ast b;
    Z3_ast c;
    Z3_ast d;
    Z3_ast e;
    Z3_ast f;
    Z3_ast g;
};


void display_sort(Z3_context c, Z3_sort ty)
{
    switch (Z3_get_sort_kind(c, ty)) {
    case Z3_UNINTERPRETED_SORT:
        printf("uninterpreted sort");
        //display_symbol(c, Z3_get_sort_name(c, ty));
        break;
    case Z3_BOOL_SORT:
        printf( "bool");
        break;
    case Z3_INT_SORT:
        printf( "int");
        break;
    case Z3_REAL_SORT:
        printf( "real");
        break;
    case Z3_BV_SORT:
        printf( "bv%d", Z3_get_bv_sort_size(c, ty));
        break;
    case Z3_ARRAY_SORT:
        printf( "[");
        display_sort(c,  Z3_get_array_sort_domain(c, ty));
        printf( "->");
        display_sort(c,  Z3_get_array_sort_range(c, ty));
        printf( "]");
        break;
    case Z3_DATATYPE_SORT:
        if (Z3_get_datatype_sort_num_constructors(c, ty) != 1)
        {
            printf( "%s", Z3_sort_to_string(c,ty));
            break;
        }
        {
            unsigned num_fields = Z3_get_tuple_sort_num_fields(c, ty);
            unsigned i;
            printf( "(");
            for (i = 0; i < num_fields; i++) {
                Z3_func_decl field = Z3_get_tuple_sort_field_decl(c, ty, i);
                if (i > 0) {
                    printf( ", ");
                }
                display_sort(c,  Z3_get_range(c, field));
            }
            printf( ")");
            break;
        }
    default:
        printf( "unknown[");
        //display_symbol(c,  Z3_get_sort_name(c, ty));
        printf( "]");
        break;
    }
}

void display_ast(Z3_context c, Z3_ast v)
{
    switch (Z3_get_ast_kind(c, v)) {
    case Z3_NUMERAL_AST: {
        printf("NO");
        Z3_sort t;
        printf("%s", Z3_get_numeral_string(c, v));
        t = Z3_get_sort(c, v);
        printf(":");
        //display_sort(c, out, t);
        break;
    }
    case Z3_APP_AST: {
        unsigned i;
        Z3_app app = Z3_to_app(c, v);
        unsigned num_fields = Z3_get_app_num_args(c, app);
        Z3_func_decl d = Z3_get_app_decl(c, app);
        printf("<<%s>>", Z3_func_decl_to_string(c, d));
        if (num_fields > 0) {
            printf("[");
            for (i = 0; i < num_fields; i++) {
                if (i > 0) {
                    printf(", ");
                }
                display_ast(c, Z3_get_app_arg(c, app, i));
            }
            printf("]");
        }
        break;
    }
    case Z3_QUANTIFIER_AST: {
        printf("quantifier");
    }
    default:
        printf("#unknown");
    }
}

int main (
    int argc,
    char const* argv[]
)
{
    // Make a config
    Z3_config config = Z3_mk_config();

    // And a context
    Z3_context ctx = Z3_mk_context(config);

    // No need for the config any more
    Z3_del_config(config);

    Z3_sort bool_sort = Z3_mk_bool_sort(ctx);


    // Each segment is denoted by a boolean symbol
//    Z3_ast top_segment = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "top_segment"), bool_sort);

    // And each of the inputs, a, b, c, d, e, f, and g, is a symbol
//    Z3_ast a = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "a"), bool_sort);
//    Z3_ast b = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "b"), bool_sort);
//    Z3_ast c = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "c"), bool_sort);
//    Z3_ast d = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "d"), bool_sort);
//    Z3_ast e = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "e"), bool_sort);
//    Z3_ast f = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "f"), bool_sort);
//    Z3_ast g = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "g"), bool_sort);


    // Now lets describe some rules, first off, the segments are all either
    // a, b, c, d, e, f, or g.
//    Z3_ast top_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, top_segment, a),
//            Z3_mk_eq(ctx, top_segment, b),
//            Z3_mk_eq(ctx, top_segment, c),
//            Z3_mk_eq(ctx, top_segment, d),
//            Z3_mk_eq(ctx, top_segment, e),
//            Z3_mk_eq(ctx, top_segment, f),
//            Z3_mk_eq(ctx, top_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );
//    Z3_ast topleft_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, topleft_segment, a),
//            Z3_mk_eq(ctx, topleft_segment, b),
//            Z3_mk_eq(ctx, topleft_segment, c),
//            Z3_mk_eq(ctx, topleft_segment, d),
//            Z3_mk_eq(ctx, topleft_segment, e),
//            Z3_mk_eq(ctx, topleft_segment, f),
//            Z3_mk_eq(ctx, topleft_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );
//    Z3_ast bottomleft_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, bottomleft_segment, a),
//            Z3_mk_eq(ctx, bottomleft_segment, b),
//            Z3_mk_eq(ctx, bottomleft_segment, c),
//            Z3_mk_eq(ctx, bottomleft_segment, d),
//            Z3_mk_eq(ctx, bottomleft_segment, e),
//            Z3_mk_eq(ctx, bottomleft_segment, f),
//            Z3_mk_eq(ctx, bottomleft_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );
//    Z3_ast topright_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, topright_segment, a),
//            Z3_mk_eq(ctx, topright_segment, b),
//            Z3_mk_eq(ctx, topright_segment, c),
//            Z3_mk_eq(ctx, topright_segment, d),
//            Z3_mk_eq(ctx, topright_segment, e),
//            Z3_mk_eq(ctx, topright_segment, f),
//            Z3_mk_eq(ctx, topright_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );
//    Z3_ast bottomright_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, bottomright_segment, a),
//            Z3_mk_eq(ctx, bottomright_segment, b),
//            Z3_mk_eq(ctx, bottomright_segment, c),
//            Z3_mk_eq(ctx, bottomright_segment, d),
//            Z3_mk_eq(ctx, bottomright_segment, e),
//            Z3_mk_eq(ctx, bottomright_segment, f),
//            Z3_mk_eq(ctx, bottomright_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );
//    Z3_ast bottom_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, bottom_segment, a),
//            Z3_mk_eq(ctx, bottom_segment, b),
//            Z3_mk_eq(ctx, bottom_segment, c),
//            Z3_mk_eq(ctx, bottom_segment, d),
//            Z3_mk_eq(ctx, bottom_segment, e),
//            Z3_mk_eq(ctx, bottom_segment, f),
//            Z3_mk_eq(ctx, bottom_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );
//    Z3_ast center_segment_one_of_input = Z3_mk_pbeq(
//        ctx,
//        7, 
//        (Z3_ast[]){
//            Z3_mk_eq(ctx, center_segment, a),
//            Z3_mk_eq(ctx, center_segment, b),
//            Z3_mk_eq(ctx, center_segment, c),
//            Z3_mk_eq(ctx, center_segment, d),
//            Z3_mk_eq(ctx, center_segment, e),
//            Z3_mk_eq(ctx, center_segment, f),
//            Z3_mk_eq(ctx, center_segment, g)
//        },
//        (int[]){1, 1, 1, 1, 1, 1, 1},
//        1
//    );


    // A zero needs 4 segments, a one needs 2, and so one...

    // Now, all of these rules need to apply

    // lets say we see an 'abcefg', then
    {
        struct signal_pattern fst = {
            .a = Z3_mk_true(ctx),
            .b = Z3_mk_true(ctx),
            .c = Z3_mk_false(ctx),
            .d = Z3_mk_false(ctx),
            .e = Z3_mk_false(ctx),
            .f = Z3_mk_false(ctx),
            .g = Z3_mk_false(ctx)
        };
        
        // We know that it's one of the digits...
        Z3_ast zero_is_6_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            6
        );
        Z3_ast one_is_2_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            2
        );
        Z3_ast two_is_5_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            5
        );
        Z3_ast three_is_5_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            5
        );
        Z3_ast four_is_4_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            4
        );
        Z3_ast five_is_5_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            5
        );
        Z3_ast six_is_6_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            6
        );
        Z3_ast seven_is_3_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            3
        );
        Z3_ast eight_is_7_segments = Z3_mk_and(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g }
        );
        Z3_ast nine_is_6_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            6
        );


        Z3_ast fst_is_a_digit = Z3_mk_atleast(
            ctx,
            9,
            (Z3_ast[]){ 
                zero_is_6_segments,
                one_is_2_segments,
                three_is_5_segments,
                four_is_4_segments,
                five_is_5_segments,
                six_is_6_segments,
                seven_is_3_segments,
                eight_is_7_segments,
                nine_is_6_segments
            },
            1
        );

        Z3_ast fst_is_0_ = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "fst is 0"), bool_sort);
        Z3_ast fst_is_1_ = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "fst is 1"), bool_sort);

        Z3_ast fst_is_0 = Z3_mk_iff(
            ctx, 
            fst_is_0_,
            zero_is_6_segments
        );
        Z3_ast fst_is_1 = Z3_mk_iff(
            ctx, 
            fst_is_1_,
            one_is_2_segments
        );

        Z3_ast fst_is_0_or_1 = Z3_mk_pbeq(
            ctx,
            2,
            (Z3_ast[2]){ fst_is_0_, fst_is_1_ },
            (int[]){1, 1},
            1
        );

        Z3_ast rules = Z3_mk_and(
            ctx,
            3, 
            (Z3_ast[]){ fst_is_0, fst_is_1, fst_is_0_or_1 }
        );

        Z3_solver solver = Z3_mk_solver(ctx);
        Z3_solver_assert(ctx, solver, rules);

        switch (Z3_solver_check(ctx, solver)) {
            case Z3_L_FALSE:
                printf("%s:%d:%s: false\n", __FILE__, __LINE__, __func__);
                return 0;

            case Z3_L_UNDEF:
                printf("%s:%d:%s: undef\n", __FILE__, __LINE__, __func__);
                return 0;

            case Z3_L_TRUE: {
                printf("%s:%d:%s: true\n", __FILE__, __LINE__, __func__);
                Z3_model model = Z3_solver_get_model(ctx, solver);
                printf("model: %s\n", Z3_model_to_string(ctx, model));

                Z3_ast result;

                if (true == Z3_model_eval(ctx, model, one_is_2_segments, true, &result)) {
                    display_ast(ctx, result);
                }

                break;
            }
        }
    }


    {

        struct signal_pattern patterns[10] = {
            // 0
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_true(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_false(ctx),
                .e = Z3_mk_true(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_true(ctx)
            },
            // 1
            {
                .a = Z3_mk_false(ctx),
                .b = Z3_mk_false(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_false(ctx),
                .e = Z3_mk_false(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_false(ctx)
            },
            // 2
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_false(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_true(ctx),
                .f = Z3_mk_false(ctx),
                .g = Z3_mk_true(ctx)
            },
            // 3
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_false(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_false(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_false(ctx)
            },
            // 4
            {
                .a = Z3_mk_false(ctx),
                .b = Z3_mk_true(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_false(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_false(ctx)
            },
            // 5
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_true(ctx),
                .c = Z3_mk_false(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_false(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_true(ctx)
            },
            // 6
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_true(ctx),
                .c = Z3_mk_false(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_true(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_true(ctx)
            },
            // 7
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_false(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_false(ctx),
                .e = Z3_mk_false(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_false(ctx)
            },
            // 8
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_true(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_true(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_true(ctx)
            },
            // 9
            {
                .a = Z3_mk_true(ctx),
                .b = Z3_mk_true(ctx),
                .c = Z3_mk_true(ctx),
                .d = Z3_mk_true(ctx),
                .e = Z3_mk_false(ctx),
                .f = Z3_mk_true(ctx),
                .g = Z3_mk_true(ctx)
            },
        };
        
        // We know that it's one of the digits...
        Z3_ast zero_is_6_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            6
        );
        Z3_ast one_is_2_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            2
        );
        Z3_ast two_is_5_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            5
        );
        Z3_ast three_is_5_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            5
        );
        Z3_ast four_is_4_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            4
        );
        Z3_ast five_is_5_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            5
        );
        Z3_ast six_is_6_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            6
        );
        Z3_ast seven_is_3_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            3
        );
        Z3_ast eight_is_7_segments = Z3_mk_and(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g }
        );
        Z3_ast nine_is_6_segments = Z3_mk_pbeq(
            ctx,
            7,
            (Z3_ast[]){ fst.a, fst.b, fst.c, fst.d, fst.e, fst.f, fst.g },
            (int[]){1, 1, 1, 1, 1, 1, 1},
            6
        );


        Z3_ast fst_is_a_digit = Z3_mk_atleast(
            ctx,
            9,
            (Z3_ast[]){ 
                zero_is_6_segments,
                one_is_2_segments,
                three_is_5_segments,
                four_is_4_segments,
                five_is_5_segments,
                six_is_6_segments,
                seven_is_3_segments,
                eight_is_7_segments,
                nine_is_6_segments
            },
            1
        );

        Z3_ast fst_is_0_ = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "fst is 0"), bool_sort);
        Z3_ast fst_is_1_ = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "fst is 1"), bool_sort);

        Z3_ast fst_is_0 = Z3_mk_iff(
            ctx, 
            fst_is_0_,
            zero_is_6_segments
        );
        Z3_ast fst_is_1 = Z3_mk_iff(
            ctx, 
            fst_is_1_,
            one_is_2_segments
        );

        Z3_ast fst_is_0_or_1 = Z3_mk_pbeq(
            ctx,
            2,
            (Z3_ast[2]){ fst_is_0_, fst_is_1_ },
            (int[]){1, 1},
            1
        );

        Z3_ast rules = Z3_mk_and(
            ctx,
            3, 
            (Z3_ast[]){ fst_is_0, fst_is_1, fst_is_0_or_1 }
        );

        Z3_solver solver = Z3_mk_solver(ctx);
        Z3_solver_assert(ctx, solver, rules);

        switch (Z3_solver_check(ctx, solver)) {
            case Z3_L_FALSE:
                printf("%s:%d:%s: false\n", __FILE__, __LINE__, __func__);
                return 0;

            case Z3_L_UNDEF:
                printf("%s:%d:%s: undef\n", __FILE__, __LINE__, __func__);
                return 0;

            case Z3_L_TRUE: {
                printf("%s:%d:%s: true\n", __FILE__, __LINE__, __func__);
                Z3_model model = Z3_solver_get_model(ctx, solver);
                printf("model: %s\n", Z3_model_to_string(ctx, model));

                Z3_ast result;

                if (true == Z3_model_eval(ctx, model, one_is_2_segments, true, &result)) {
                    display_ast(ctx, result);
                }

                break;
            }
        }
    }


//    Z3_ast top = Z3_mk_true(ctx);
//    Z3_ast bottom = Z3_mk_false(ctx);
//
//    Z3_ast x = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "x"), Z3_mk_bool_sort(ctx));
//
//    Z3_ast or = Z3_mk_xor(ctx, top, x);
//
//    Z3_ast eq = Z3_mk_eq(ctx, top, or);


//    Z3_solver solver = Z3_mk_solver(ctx);
//    Z3_solver_assert(ctx, solver, rules);

//    switch (Z3_solver_check(ctx, solver)) {
//        case Z3_L_FALSE:
//            printf("%s:%d:%s: false\n", __FILE__, __LINE__, __func__);
//            return 0;
//
//        case Z3_L_UNDEF:
//            printf("%s:%d:%s: undef\n", __FILE__, __LINE__, __func__);
//            return 0;
//
//        case Z3_L_TRUE:
//            printf("%s:%d:%s: true\n", __FILE__, __LINE__, __func__);
//            break;
//    }
//
//    Z3_model model = Z3_solver_get_model(ctx, solver);
//    printf("model: %s\n", Z3_model_to_string(ctx, model));
    printf("\n");

    return 0;
}
