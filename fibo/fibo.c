#include <gmp.h>
#include <getopt.h>
#include <stdlib.h>
#include <sysexits.h>
#include <stdio.h>
#include <sys/rbtree.h>

/*
 * Ever wondered what the Nth Fibonacci Sequence number is? This'll tell ya!
 *
 * A key concern is that as N gets larger, F(N) grows exponentially, making
 * each mathematical operation take O(2^n) fixed-integer-precision
 * modulo-arithmetic operations.
 *
 * This means an O(log(n)) _operations_ algorithm actually scales as O(n).
 *
 * Should build with no errors with the following command:
 * gcc -std=c99 -Wall -Wextra -pedantic -O3 -l gmp -o fibo fibo.c 
 */

/* memoization tree node */
struct memo {
  rb_node_t rb; /* used by rbtree */
  unsigned int n; /* the index of this fibonacci number */
  mpz_t f_n; /* the fibonacci number */
};

/* Non-explosive algorithm, O(1) space, O(n) operations */
void fibo_naive(unsigned int x);
/* Better algorithm, O(log(n)) space, O(log(n)) operations */
void fibo_smart(unsigned int n);
mpz_t *fibo_smart_helper(rb_tree_t *memo, unsigned int n);
signed int compare_memo_nodes(void *ctx, const struct memo *node1, const struct memo *node2);
signed int compare_memo_node_key(void *ctx, const struct memo *node, const unsigned int *key);

static rb_tree_ops_t memo_tree_ops = {
  .rbto_compare_nodes = (rbto_compare_nodes_fn) compare_memo_nodes,
  .rbto_compare_key = (rbto_compare_key_fn) compare_memo_node_key,
  .rbto_node_offset = 0,
  .rbto_context = NULL
};

/* The implementation of the fibonacci number function we will run. */
static void (*fibo_impl)(unsigned int) = fibo_naive;
/* 0 -> no output, 1 -> prints the number on stdout */
static int output = 1;

/* Calculate the Nth fibonacci number */

int main(int argc, char * const argv[]) {
  struct option getopts[] = {
    {"quiet", no_argument, NULL, 'q'},
    {"smart", no_argument, NULL, 's'}
  };
  unsigned int n = 0;
  int ch;
  while((ch=getopt_long(argc, argv, "qs", getopts, NULL)) != -1) {
    switch(ch) {
      case 'q':
        output = 0;
        break;
      case 's':
        fibo_impl = fibo_smart;
        break;
      default:
        fprintf(stderr, "usage: fibo [-q|--quiet][-s|--smart] N \n");
        return EX_USAGE;
    }
  }
  if (sscanf(argv[argc-1], "%u", &n) != 1) {
    fprintf(stderr, "usage: fibo [-q|--quiet][-s|--smart] N \n");
    return EX_USAGE;
  }
  fibo_impl(n);
  return EX_OK;
}

void fibo_naive(unsigned int x) {
  mpz_t f_nm2, f_nm1, f_n;
  if (x == 0 || x == 1) {
    if (output) {
      printf("fibo(%d): %d\n", x, x);
    }
    return;
  }
  mpz_init_set_ui(f_nm2, 0);
  mpz_init_set_ui(f_nm1, 1);
  mpz_init(f_n);
  for (unsigned int i=1; i<x; ++i) {
    mpz_add(f_n, f_nm2, f_nm1);
    mpz_set(f_nm2, f_nm1);
    mpz_set(f_nm1, f_n);
  }
  if (output) {
    gmp_printf("fibo(%d): %Zd\n", x, f_n);
  }
  mpz_clear(f_n);
  mpz_clear(f_nm1);
  mpz_clear(f_nm2);
}

void fibo_smart(unsigned int n) {
  rb_tree_t memo;
  rb_tree_init(&memo, &memo_tree_ops);
  mpz_t *result;
  struct memo n_0, n_1;
  /* initialize f(0) and f(1) */
  n_0.n = 0;
  mpz_init_set_ui(n_0.f_n, 0);
  n_1.n = 1;
  mpz_init_set_ui(n_1.f_n, 1);
  rb_tree_insert_node(&memo, &n_0);
  rb_tree_insert_node(&memo, &n_1);
  /* ju li, do the thing */
  result = fibo_smart_helper(&memo, n);
  if (output) {
    gmp_printf("fibo(%d): %Zd\n", n, *result);
  }
  /*de-alloc all memoized nodes and mpz_t*/
  rb_tree_remove_node(&memo, &n_0);
  rb_tree_remove_node(&memo, &n_1);
  mpz_clear(n_0.f_n);
  mpz_clear(n_1.f_n);
  struct memo *curr; 
  while ((curr = rb_tree_iterate(&memo, NULL, RB_DIR_RIGHT)) != NULL) {
    rb_tree_remove_node(&memo, curr);
    mpz_clear(curr->f_n);
    free(curr);
  }
}

/* With memoization, use the fast recursive fibonacci formulae:
 *
 * F(2n-1) = F(n)^2 + F(n+1)^2
 * F(2n) = (2*F(n-1) + F(n)) * F(n)
 */
mpz_t *fibo_smart_helper(rb_tree_t *memo, unsigned int n) {
  struct memo *node;
  if (!(node = (struct memo *)rb_tree_find_node(memo, &n))) {
    node = (struct memo *) malloc(sizeof(struct memo));
    node->n = n;
    mpz_init(node->f_n);
    if (n%2 == 0) { /* N is even */
      mpz_t *f_n = fibo_smart_helper(memo, n/2);
      mpz_t *f_nm1 = fibo_smart_helper(memo, n/2 - 1);
      mpz_set(node->f_n, *f_nm1);
      mpz_mul_ui(node->f_n, node->f_n, 2);
      mpz_add(node->f_n, node->f_n, *f_n);
      mpz_mul(node->f_n, node->f_n, *f_n);
    } else { /* N is odd */
      mpz_t *f_n = fibo_smart_helper(memo, n/2);
      mpz_t *f_np1 = fibo_smart_helper(memo, n/2 + 1);
      mpz_t scratch;
      mpz_init_set(scratch, *f_n);
      mpz_set(node->f_n, *f_np1);
      mpz_pow_ui(scratch, scratch, 2);
      mpz_pow_ui(node->f_n, node->f_n, 2);
      mpz_add(node->f_n, node->f_n, scratch);
      mpz_clear(scratch);
    }
    rb_tree_insert_node(memo, node);
  }
  return &node->f_n;
}

signed int compare_memo_nodes(void *ctx, const struct memo *node1, const struct memo *node2) {
  return compare_memo_node_key(ctx, node1, &node2->n);
}

signed int compare_memo_node_key(void *ctx, const struct memo *node, const unsigned int *key) {
  (void) ctx; /* -Wunused-parameter */
  return (signed)node->n - (signed)*key;
}

