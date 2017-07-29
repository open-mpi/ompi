/*
 * Copyright (c) 2017 Amazon.com, Inc. or its affiliates.  All Rights
 *                    reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"

#include "reachable_shared.h"

/* sigh; needs to match with code in reachable_weighted, but those
   headers aren't installed. */
enum connection_quality {
    CQ_NO_CONNECTION = 0,
    CQ_PRIVATE_DIFFERENT_NETWORK = 50,
    CQ_PRIVATE_SAME_NETWORK = 80,
    CQ_PUBLIC_DIFFERENT_NETWORK = 90,
    CQ_PUBLIC_SAME_NETWORK = 100
};


/* SUITE 1:
 * Tests IPv4 connections by
 * modifying ip addresses and
 * subnet masks.  Also tests
 * IPv4->IPv6 and the other way
 * around, to assure no connection
 * is returned in that case.
 */
int ipv4_test()
{
    opal_if_t *int1;
    opal_if_t *int2;
    int expected_result;
    int result;
    int test_no = 0;
    int failed_no = 0;

    /* TEST1
     * Localhost to localhost.  Since localhost range is not a
     * private network (RFC1918), expected result is public
     * same network.
     */
    test_no++;
    expected_result = CQ_PUBLIC_SAME_NETWORK;
    int1 = create_if(AF_INET, "127.0.0.1", 24, 0);
    int2 = create_if(AF_INET, "127.0.0.2", 0, 0);
    result = run_single_test(int1, int2);
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST2
     * Testing public same network with subnet mask
     * 255.255.255.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_SAME_NETWORK;
    int1 = create_if(AF_INET, "31.14.15.92", 24, 0);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST3
     * Testing public same network with subnet mask
     * 255.255.0.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_SAME_NETWORK;
    int1 = create_if(AF_INET, "65.35.89.79", 16, 0);
    int2 = create_if(AF_INET, "65.35.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST4
     * Testing public same network with subnet mask
     * 255.0.0.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_SAME_NETWORK;
    int1 = create_if(AF_INET, "3.23.84.62", 8, 0);
    int2 = create_if(AF_INET, "3.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST5
     * Testing public same network with subnet mask
     * 0.0.0.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_SAME_NETWORK;
    int1 = create_if(AF_INET, "64.33.83.27", 0, 0);
    int2 = create_if(AF_INET, "27.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST6
     * Testing public different network with subnet mask
     * 255.255.255.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET, "95.2.88.41", 24, 0);
    int2 = create_if(AF_INET, "95.2.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST7
     * Testing public different network with subnet mask
     * 255.255.0.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET, "97.16.93.99", 16, 0);
    int2 = create_if(AF_INET, "97.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST8
     * Testing public different network with subnet mask
     * 255.0.0.0
     */
    test_no++;
    expected_result = CQ_PUBLIC_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET, "37.51.5.82", 8, 0);
    int2 = create_if(AF_INET, "27.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST9
     * Testing private same network with subnet mask
     * 255.255.255.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_SAME_NETWORK;
    int1 = create_if(AF_INET, "192.168.0.1", 24, 0);
    int2 = create_if(AF_INET, "192.168.0.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST10
     * Testing private same network with subnet mask
     * 255.255.0.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_SAME_NETWORK;
    int1 = create_if(AF_INET, "192.168.0.1", 16, 0);
    int2 = create_if(AF_INET, "192.168.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed this test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST11
     * Testing private same network with subnet mask
     * 255.0.0.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_SAME_NETWORK;
    int1 = create_if(AF_INET, "172.16.0.1", 8, 0);
    int2 = create_if(AF_INET, "172.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed this test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST12
     * Testing private same network with subnet mask
     * 0.0.0.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_SAME_NETWORK;
    int1 = create_if(AF_INET, "192.168.0.1", 0, 0);
    int2 = create_if(AF_INET, "10.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST13
     * Testing private different network with subnet mask
     * 255.255.255.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET, "192.168.0.1", 24, 0);
    int2 = create_if(AF_INET, "192.168.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST14
     * Testing private different network with subnet mask
     * 255.255.0.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET, "192.168.0.1", 16, 0);
    int2 = create_if(AF_INET, "10.1.0.1", 16, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST15
     * Testing private different network with subnet mask
     * 255.0.0.0
     */
    test_no++;
    expected_result = CQ_PRIVATE_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET, "192.168.0.1", 8, 0);
    int2 = create_if(AF_INET, "10.27.27.27", 0, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST16
     * Testing public to private with subnet mask
     * 255.255.255.0
     */
    test_no++;
    expected_result = CQ_NO_CONNECTION;
    int1 = create_if(AF_INET, "27.27.27.27", 24 , 0);
    int2 = create_if(AF_INET, "192.168.0.1", 16, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST17
     * Testing private to public with subnet mask
     * 255.255.255.0
     */
    test_no++;
    expected_result = CQ_NO_CONNECTION;
    int1 = create_if(AF_INET, "192.168.0.1", 24, 0);
    int2 = create_if(AF_INET, "27.27.27.27", 8, 0);
    result = run_single_test(int1, int2);

    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST18
     * IPv4->IPv6
     */
    expected_result = CQ_NO_CONNECTION;
    int1 = create_if(AF_INET, "8.8.8.8", 24, 0);
    int2 = create_if(AF_INET6, "2001:4860:4860:0:0:0:0:8888", 0, 0);
    result = run_single_test(int1, int2);
    test_no++;
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST19
     * IPv6->IPv4
     */
    expected_result = CQ_NO_CONNECTION;
    int1 = create_if(AF_INET6, "2001:4860:4860:0:0:0:0:8888", 64, 0);
    int2 = create_if(AF_INET, "8.8.8.8", 0, 0);
    result = run_single_test(int1, int2);
    test_no++;
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    opal_output(0, "Finished Reachable IPv4 Tests.  %d/%d successful", test_no-failed_no, test_no);

    if (0 == failed_no) {
	return 0;
    } else {
	return 1;
    }
}


/* SUITE 2:
 * Compares connections with different
 * bandwidths to see ensure the
 * relative ranking is as expected
 */
int ranking_test()
{
    opal_if_t *int1;
    opal_if_t *int2;
    int result1;
    int result2;
    int test_no = 0;
    int failed_no = 0;

    /* TEST1
     * Compares pairs with bandwidths 0->0 and 1->0.
     * The former connection should be better, as
     * there is a smaller difference in bandwidth
     * (This is an edge case, but this behavior makes
     * sense.  We want 0->0 to still work, incase
     * bandwidth was never set.  Thus, the behavior
     * for a->b where a = 0 and a != b should
     * act the same as any other case, where
     * a greater difference in leads to a greater
     * penalty in bandwidth)
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 0);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 0);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 0);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 > result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST2
     * Compares interface pairs with bandwidth 0->0 and 1->2.
     * The latter should be better as it has greater bandwidth
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 0);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 0);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 2);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST3
     * Compares interface pairs with bandwidth 1->2 and 1->1.
     * The later should be better as there is a smaller
     * difference in bandwidth
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 0);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 0);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 1);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST4
     * Compares interface pairs with bandwidth 1->3 and 1->2.
     * The later should be better as there is a smaller
     * difference in bandwidth
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 3);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 2);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST5
     * Compares interface pairs with bandwidth 1->10 and 1->3.
     * The later should be better as there is less discrepancy
     * in bandwidth
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 10);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 1);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 3);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST6
     * Compares interface pairs with bandwidth 5->5 and 10->10.
     * The later should be better as it has higher bandwidth
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 5);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 5);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 10);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 10);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST7
     * Compares interface pairs with bandwidth 10->11 and 10->10.
     * The later should be better as there is no discrepancy in
     * bandwidth.
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 10);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 11);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 10);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 10);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST8
     * Compares interface pairs with bandwidth 10->11 and 11->10.
     * These connections should be equivilant, as they have the same
     * bandwidth and same discrepancy.
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 10);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 11);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 11);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 10);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 == result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    /* TEST9
     * Compares interface pairs with bandwidth 10->14 and 11->15.
     * The latter should be better as it has higher bandwidth.
     */
    test_no++;

    int1 = create_if(AF_INET, "31.14.15.92", 24, 10);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 14);
    result1 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    int1 = create_if(AF_INET, "31.14.15.92", 24, 11);
    int2 = create_if(AF_INET, "31.14.15.27", 0, 15);
    result2 = run_single_test(int1, int2);
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    if (!(result1 < result2)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }

    opal_output(0, "Finished Reachable Weighted Ranking Tests.  %d/%d successful", test_no-failed_no, test_no);
    if (0 == failed_no) {
	return 0;
    } else {
	return 1;
    }
}


/* SUITE 3:
 * Tests interfaces lists of various sizes
 * to ensure no crashes occur and results
 * are outputted in proper order
 */
int loop_test()
{
    int test_no = 0;
    int failed_no = 0;

    opal_list_t *if_list1, *if_list2;
    opal_if_t *intf;
    opal_reachable_t *results;

    int i;

    /* TEST1:
     * Ensure opal_reachable doesn't crash
     * when called with empty lists
     */
    test_no++;

    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);

    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(0 == results->num_local && 0 == results->num_remote)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST2:
     * Ensure opal_reachable doesn't crash
     * when called with empty local list
     */
    test_no++;

    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);

    intf = create_if(AF_INET, "31.14.19.92", 24, 0);
    opal_list_append(if_list2, &(intf->super));
    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(0 == results->num_local && 1 == results->num_remote)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST3:
     * Ensure opal_reachable doesn't crash
     * when called with empty remote list
     */
    test_no++;

    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);
    intf = create_if(AF_INET, "31.14.19.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));
    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(1 == results->num_local && 0 == results->num_remote)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST4:
     * Ensure opal_reachable doesn't crash
     * when the remote list has more elements
     * than the local list
     */
    test_no++;

    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);
    for (i = 0; i < 3; i++) {
	intf = create_if(AF_INET, "31.14.19.92", 24, 0);
	opal_list_append(if_list1, &(intf->super));
    }
    for (i = 0; i < 14; i++) {
	intf = create_if(AF_INET, "31.14.19.92", 24, 0);
	opal_list_append(if_list2, &(intf->super));
    }
    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(3 == results->num_local && 14 == results->num_remote)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST5:
     * Ensure opal_reachable doesn't crash
     * when the local list has more elements
     * than the remote list
     */
    test_no++;

    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);
    for (i = 0; i < 14; i++) {
	intf = create_if(AF_INET, "31.14.19.92", 24, 0);
	opal_list_append(if_list1, &(intf->super));
    }
    for (i = 0; i < 3; i++) {
	intf = create_if(AF_INET, "31.14.19.92", 24, 0);
	opal_list_append(if_list2, &(intf->super));
    }
    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(14 == results->num_local && 3 == results->num_remote)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST6:
     * Ensure opal_reachable doesn't crash
     * when the local list has the same number
     * of elements as the remote list
     */
    test_no++;

    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);
    for (i = 0; i < 27; i++) {
	intf = create_if(AF_INET, "31.14.19.92", 24, 0);
	opal_list_append(if_list1, &(intf->super));
    }
    for (i = 0; i < 27; i++) {
	intf = create_if(AF_INET, "31.14.19.92", 24, 0);
	opal_list_append(if_list2, &(intf->super));
    }
    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(27 == results->num_local && 27 == results->num_remote)) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST7:
     * Tests proper ordering of results
     * when same number of local interfaces and
     * remote interfaces
     */
    test_no++;
    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);

    intf = create_if(AF_INET, "31.14.19.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));
    intf = create_if(AF_INET, "31.14.20.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));

    intf = create_if(AF_INET, "31.14.19.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));
    intf = create_if(AF_INET, "31.14.20.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));

    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(CQ_PUBLIC_SAME_NETWORK == results->weights[0][0] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[0][1] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[1][0] &&
	  CQ_PUBLIC_SAME_NETWORK == results->weights[1][1])) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST8:
     * Tests proper ordering of results
     * when greater number of remote interfaces
     * than local interfaces
     */
    test_no++;
    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);

    intf = create_if(AF_INET, "31.14.19.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));
    intf = create_if(AF_INET, "31.14.20.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));

    intf = create_if(AF_INET, "31.14.19.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));
    intf = create_if(AF_INET, "31.14.20.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));
    intf = create_if(AF_INET, "31.14.21.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));

    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(CQ_PUBLIC_SAME_NETWORK == results->weights[0][0] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[0][1] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[0][2] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[1][0] &&
	  CQ_PUBLIC_SAME_NETWORK == results->weights[1][1]) &&
	CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[1][2]) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    /* TEST9:
     * Tests proper ordering of results
     * when greater number of local interfaces
     * than remote interfaces
     */
    test_no++;
    if_list1 = OBJ_NEW(opal_list_t);
    if_list2 = OBJ_NEW(opal_list_t);

    intf = create_if(AF_INET, "31.14.19.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));
    intf = create_if(AF_INET, "31.14.20.92", 24, 0);
    opal_list_append(if_list1, &(intf->super));
    intf = create_if(AF_INET, "31.14.21.93", 24, 0);
    opal_list_append(if_list1, &(intf->super));

    intf = create_if(AF_INET, "31.14.19.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));
    intf = create_if(AF_INET, "31.14.20.93", 24, 0);
    opal_list_append(if_list2, &(intf->super));

    results = opal_reachable.reachable(if_list1, if_list2);

    OBJ_RELEASE(if_list1);
    OBJ_RELEASE(if_list2);

    if (!(CQ_PUBLIC_SAME_NETWORK == results->weights[0][0] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[0][1] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[1][0] &&
	  CQ_PUBLIC_SAME_NETWORK == results->weights[1][1] &&
	  CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[2][0]) &&
	CQ_PUBLIC_DIFFERENT_NETWORK == results->weights[2][1]) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(results);

    opal_output(0, "Finished Reachable Weighted Loop Tests.  %d/%d successful", test_no-failed_no, test_no);

    if (0 == failed_no) {
	return 0;
    } else {
	return 1;
    }

}


/* SUITE 4:
 * Test IPv6
 */
int test_ipv6()
{
    int failed_no = 0;

#if OPAL_ENABLE_IPV6
    opal_if_t *int1;
    opal_if_t *int2;
    int expected_result;
    int result;
    int test_no = 0;

    /* TEST1
     * Testing ipv6 same network with subnet mask
     * \64
     */
    expected_result = CQ_PUBLIC_SAME_NETWORK;
    int1 = create_if(AF_INET6, "2001:4860:4860:0:0:0:0:8888", 64, 0);
    int2 = create_if(AF_INET6, "2001:4860:4860:0:0:0:0:8889", 8, 0);
    result = run_single_test(int1, int2);
    test_no++;
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    /* TEST2
     * Testing ipv6 different network with subnet mask
     * \64
     */
    expected_result = CQ_PUBLIC_DIFFERENT_NETWORK;
    int1 = create_if(AF_INET6, "2001:4860:4860:0:0:0:0:8888", 64, 0);
    int2 = create_if(AF_INET6, "2001:4860:4860:1:0:0:0:8888", 0, 0);
    result = run_single_test(int1, int2);
    test_no++;
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    expected_result = CQ_PRIVATE_SAME_NETWORK;
    int1 = create_if(AF_INET6, "fe80::8888", 64, 0);
    int2 = create_if(AF_INET6, "fe80::8889", 64, 0);
    result = run_single_test(int1, int2);
    test_no++;
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    expected_result = CQ_NO_CONNECTION;
    int1 = create_if(AF_INET6, "2001::8888", 64, 0);
    int2 = create_if(AF_INET6, "fe80::8889", 64, 0);
    result = run_single_test(int1, int2);
    test_no++;
    if (result != expected_result) {
	++failed_no;
	opal_output(0, "Failed test #%d", test_no);
    }
    OBJ_RELEASE(int1);
    OBJ_RELEASE(int2);

    opal_output(0, "Finished Reachable Weighted IPv6 Tests.  %d/%d successful", test_no-failed_no, test_no);

    if (0 == failed_no) {
	return 0;
    } else {
	return 1;
    }
#else
    opal_output(0, "No IPv6 support; skipped tests");
    return 0;
#endif
}

int main(int argc, char **argv)
{
    int failed = 0;
    int total = 0;

    opal_init(&argc, &argv);
    opal_output(0, "\n\nBeginning Reachable Weighted tests\n\n");

    total++;
    if (ipv4_test()) {
	failed++;
    }

    total++;
    if (ranking_test()) {
	failed++;
    }

    total++;
    if (loop_test()) {
	failed++;
    }

    total++;
    if (test_ipv6()) {
	failed++;
    }

    if (0 != failed) {
	opal_output(0, "\n\nFailed %d/%d Reachable Weighted Test Suites :(\n\n", failed, total);	
    } else {
	opal_output(0, "\n\nPassed %d/%d Reachable Weighted Test Suites :)\n\n", total, total);	
    }

    return failed;
}
