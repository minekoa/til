
/**
 * Objectっぽい構造体。これをCallbackしよう
 */
typedef struct _FooObj{
    int field1;
    int field2;

    int (*staticMethod1)(int a1);
    int (*method1)(_FooSt *self, int a1);

} FooObj;


int foo_StaticMethod1(int a) {
    return a + 1;
}

int foo_Method1(FooObj* self, int a) {
    return self->feild1 + self->feild2 + a;
}

FooObj fooObj_new() {
    FooObj* foo = malloc(sizeof(FooObj));

    foo->field1        = 1;
    foo->field2        = 2;
    foo->staticMethod1 = foo_staticMethod1;
    foo->method1       = foo_method1;

    return foo;
};

void fooObj_delete(FooObj* self) {
    free(self);
}



