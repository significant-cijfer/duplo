#include <stdlib.h>
#include <stdio.h>

int *boxInt(int n) {
    int *ptr = malloc(sizeof(int));
    *ptr = n;
    return ptr;
}

void boxPrint(int *n) {
    printf("boxPrint.ptr: 0x%lx\n", n);
    printf("boxPrint.val:   %d\n", *n);
}

void *createUint8Array(int n) {
    return malloc(n);
}
