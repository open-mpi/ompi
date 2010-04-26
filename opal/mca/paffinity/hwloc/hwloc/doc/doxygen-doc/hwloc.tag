<?xml version='1.0' encoding='ISO-8859-1' standalone='yes' ?>
<tagfile>
  <compound kind="page">
    <name>index</name>
    <title>Hardware Locality</title>
    <filename>index</filename>
    <docanchor file="index">Introduction</docanchor>
    <docanchor file="index">installation</docanchor>
    <docanchor file="index">examples</docanchor>
    <docanchor file="index">interface_example</docanchor>
    <docanchor file="index">interface</docanchor>
    <docanchor file="index">history</docanchor>
    <docanchor file="index">bugs</docanchor>
  </compound>
  <compound kind="page">
    <name>termsanddefs</name>
    <title>Terms and Definitions</title>
    <filename>a00001</filename>
  </compound>
  <compound kind="page">
    <name>tools</name>
    <title>Command-line tools</title>
    <filename>a00002</filename>
    <docanchor file="a00002">cli_hwloc_bind</docanchor>
    <docanchor file="a00002">cli_hwloc_distrib</docanchor>
    <docanchor file="a00002">cli_lstopo</docanchor>
    <docanchor file="a00002">cli_hwloc_calc</docanchor>
  </compound>
  <compound kind="page">
    <name>envvar</name>
    <title>Environment variables</title>
    <filename>a00003</filename>
  </compound>
  <compound kind="page">
    <name>interoperability</name>
    <title>Interoperability with other software</title>
    <filename>a00004</filename>
  </compound>
  <compound kind="page">
    <name>threadsafety</name>
    <title>Thread safety</title>
    <filename>a00005</filename>
  </compound>
  <compound kind="page">
    <name>embed</name>
    <title>Embedding hwloc in other software</title>
    <filename>a00006</filename>
    <docanchor file="a00006">embedding_example</docanchor>
    <docanchor file="a00006">embedding_m4</docanchor>
  </compound>
  <compound kind="page">
    <name>switchfromplpa</name>
    <title>Switching from PLPA to hwloc</title>
    <filename>a00007</filename>
    <docanchor file="a00007">switchfromplpa_counting</docanchor>
    <docanchor file="a00007">switchfromplpa_indexes</docanchor>
    <docanchor file="a00007">switchfromplpa_caching</docanchor>
    <docanchor file="a00007">switchfromplpa_hierarchy</docanchor>
  </compound>
  <compound kind="group">
    <name>hwlocality_api_version</name>
    <title>API version</title>
    <filename>a00026.html</filename>
    <member kind="define">
      <type>#define</type>
      <name>HWLOC_API_VERSION</name>
      <anchorfile>a00026.html</anchorfile>
      <anchor>ga8f4dfb8eef138af55dd1a0fa802e5476</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_topology</name>
    <title>Topology context</title>
    <filename>a00027.html</filename>
    <member kind="typedef">
      <type>struct hwloc_topology *</type>
      <name>hwloc_topology_t</name>
      <anchorfile>a00027.html</anchorfile>
      <anchor>ga9d1e76ee15a7dee158b786c30b6a6e38</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_types</name>
    <title>Topology Object Types</title>
    <filename>a00028.html</filename>
    <member kind="enumeration">
      <name>hwloc_obj_type_t</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>gacd37bb612667dc437d66bfb175a8dc55</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_SYSTEM</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55a3aa1b842d1fd4207ebce171f95a244ec</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_MACHINE</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55a3f4e83ffc4a259354959ae8a9eaa2a80</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_NODE</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55aaf0964881117bdedf1a5e9332cd120dd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_SOCKET</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55a1ac6e07775ae4324b3fe9dbd72c785ec</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_CACHE</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55a56ee0b7eca88f363b75b34fdde8c9ddc</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_CORE</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55ac793958f330bca371aa1535de8aff45f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_PU</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55abca6887e80cb291353b0a0c1da83f661</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_GROUP</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55a5269ef95be72f88465559d35c9b7ad56</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_OBJ_MISC</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ggacd37bb612667dc437d66bfb175a8dc55a19f8a6953fa91efc76bcbcdf2d22de4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>hwloc_compare_types_e</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>ga46323568968005137c32f6a1cd405b74</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_TYPE_UNORDERED</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>gga46323568968005137c32f6a1cd405b74a2f8297ea36eba46e7596e810a67298fb</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_compare_types</name>
      <anchorfile>a00028.html</anchorfile>
      <anchor>gabd7da4f4ea12b420b8ecbde458b27805</anchor>
      <arglist>(hwloc_obj_type_t type1, hwloc_obj_type_t type2) __hwloc_attribute_const</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_objects</name>
    <title>Topology Objects</title>
    <filename>a00029.html</filename>
    <class kind="struct">hwloc_obj_memory_s</class>
    <class kind="struct">hwloc_obj</class>
    <class kind="union">hwloc_obj_attr_u</class>
    <member kind="typedef">
      <type>struct hwloc_obj *</type>
      <name>hwloc_obj_t</name>
      <anchorfile>a00029.html</anchorfile>
      <anchor>ga79b8ab56877ef99ac59b833203391c7d</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_creation</name>
    <title>Create and Destroy Topologies</title>
    <filename>a00030.html</filename>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_init</name>
      <anchorfile>a00030.html</anchorfile>
      <anchor>ga5c2d6f476af87005c7bd0811d4548b9f</anchor>
      <arglist>(hwloc_topology_t *topologyp)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_load</name>
      <anchorfile>a00030.html</anchorfile>
      <anchor>ga91e2e6427b95fb7339c99dbbef996e71</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_topology_destroy</name>
      <anchorfile>a00030.html</anchorfile>
      <anchor>ga6040925d3ee4bbb2647f2a321aca5f4b</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_topology_check</name>
      <anchorfile>a00030.html</anchorfile>
      <anchor>gab3628b2a540a5a08e8cf724ef829e70a</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_configuration</name>
    <title>Configure Topology Detection</title>
    <filename>a00031.html</filename>
    <class kind="struct">hwloc_topology_discovery_support</class>
    <class kind="struct">hwloc_topology_cpubind_support</class>
    <class kind="struct">hwloc_topology_support</class>
    <member kind="enumeration">
      <name>hwloc_topology_flags_e</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>gada025d3ec20b4b420f8038d23d6e7bde</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ggada025d3ec20b4b420f8038d23d6e7bdea129b4fea1300be22bbaf0bb0958994c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ggada025d3ec20b4b420f8038d23d6e7bdea6ecb6abc6a0bb75e81564f8bca85783b</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_ignore_type</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>gaf2071c8621fddc53649c245d87835b47</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_ignore_type_keep_structure</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ga6ddd4213d95bd1c30555b294a60efa6b</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_ignore_all_keep_structure</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>gaec6fb00050f50cd41007f1ae580d2106</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_set_flags</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ga6d11e53db143ac39c32cdb3912b71f99</anchor>
      <arglist>(hwloc_topology_t topology, unsigned long flags)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_set_fsroot</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ga2f6bfb6958d8b508ea1d7d5bb266432c</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology, const char *__hwloc_restrict fsroot_path)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_set_pid</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>gae1100de0162b3c6a9db750ac14629c05</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology, hwloc_pid_t pid)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_set_synthetic</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ga2fcb52181b586c20f001b7a999550324</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology, const char *__hwloc_restrict description)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_set_xml</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>ga93efcc8a962afe1ed23393700682173f</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology, const char *__hwloc_restrict xmlpath)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC struct hwloc_topology_support *</type>
      <name>hwloc_topology_get_support</name>
      <anchorfile>a00031.html</anchorfile>
      <anchor>gac2126e105f3ae708efca2e90d612625a</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_tinker</name>
    <title>Tinker with topologies.</title>
    <filename>a00032.html</filename>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_topology_export_xml</name>
      <anchorfile>a00032.html</anchorfile>
      <anchor>ga1c9d8e608232206ce2142fe806a6835b</anchor>
      <arglist>(hwloc_topology_t topology, const char *xmlpath)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_obj_t</type>
      <name>hwloc_topology_insert_misc_object_by_cpuset</name>
      <anchorfile>a00032.html</anchorfile>
      <anchor>ga017a9ba16d554326c6e3812d545d7230</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, const char *name)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_obj_t</type>
      <name>hwloc_topology_insert_misc_object_by_parent</name>
      <anchorfile>a00032.html</anchorfile>
      <anchor>gadacd7a3d21220fbb30c3256d8b22a294</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_t parent, const char *name)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_information</name>
    <title>Get some Topology Information</title>
    <filename>a00033.html</filename>
    <member kind="enumeration">
      <name>hwloc_get_type_depth_e</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>gaf4e663cf42bbe20756b849c6293ef575</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_TYPE_DEPTH_UNKNOWN</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>ggaf4e663cf42bbe20756b849c6293ef575a0565ab92ab72cb0cec91e23003294aad</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_TYPE_DEPTH_MULTIPLE</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>ggaf4e663cf42bbe20756b849c6293ef575ae99465995cacde6c210d5fc2e409798c</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC unsigned</type>
      <name>hwloc_topology_get_depth</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>ga8c30b0cec55074eb3ed34e4f2a1a9937</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_get_type_depth</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>gaea7c64dd59467f5201ba87712710b14d</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_obj_type_t</type>
      <name>hwloc_get_depth_type</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>gadd4964764ae7e49231065d58a553fd31</anchor>
      <arglist>(hwloc_topology_t topology, unsigned depth) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC unsigned</type>
      <name>hwloc_get_nbobjs_by_depth</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>ga20cfe2456f4cfdd789c9aca6d2fdd69f</anchor>
      <arglist>(hwloc_topology_t topology, unsigned depth) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int __hwloc_attribute_pure</type>
      <name>hwloc_get_nbobjs_by_type</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>ga0131ab1051011fabfa69d7c1853e716c</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_topology_is_thissystem</name>
      <anchorfile>a00033.html</anchorfile>
      <anchor>ga0d109e33fc7990f62f665d336e5e5111</anchor>
      <arglist>(hwloc_topology_t __hwloc_restrict topology) __hwloc_attribute_pure</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_traversal</name>
    <title>Retrieve Objects</title>
    <filename>a00034.html</filename>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_obj_t</type>
      <name>hwloc_get_obj_by_depth</name>
      <anchorfile>a00034.html</anchorfile>
      <anchor>gaedd78240b0c1108355586a268ec5a697</anchor>
      <arglist>(hwloc_topology_t topology, unsigned depth, unsigned idx) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_obj_by_type</name>
      <anchorfile>a00034.html</anchorfile>
      <anchor>ga701f83b2cf0cb8e0acd58cd2dc1c67a2</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type, unsigned idx)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_conversion</name>
    <title>Object/String Conversion</title>
    <filename>a00035.html</filename>
    <member kind="function">
      <type>HWLOC_DECLSPEC const char *</type>
      <name>hwloc_obj_type_string</name>
      <anchorfile>a00035.html</anchorfile>
      <anchor>ga7c61920feca6fd9006d930dabfc09058</anchor>
      <arglist>(hwloc_obj_type_t type) __hwloc_attribute_const</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_obj_type_t</type>
      <name>hwloc_obj_type_of_string</name>
      <anchorfile>a00035.html</anchorfile>
      <anchor>gade722091ae392fdc79557e797a16c370</anchor>
      <arglist>(const char *string) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_obj_type_snprintf</name>
      <anchorfile>a00035.html</anchorfile>
      <anchor>ga3ad856e8f3487d340c82a23b8a2a0351</anchor>
      <arglist>(char *__hwloc_restrict string, size_t size, hwloc_obj_t obj, int verbose)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_obj_attr_snprintf</name>
      <anchorfile>a00035.html</anchorfile>
      <anchor>ga0db8286d7f3ceda8defd76e3e1e2b284</anchor>
      <arglist>(char *__hwloc_restrict string, size_t size, hwloc_obj_t obj, const char *__hwloc_restrict separator, int verbose)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_obj_snprintf</name>
      <anchorfile>a00035.html</anchorfile>
      <anchor>ga5c6a61a83f4790b421e2f62e9088446f</anchor>
      <arglist>(char *__hwloc_restrict string, size_t size, hwloc_topology_t topology, hwloc_obj_t obj, const char *__hwloc_restrict indexprefix, int verbose)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_obj_cpuset_snprintf</name>
      <anchorfile>a00035.html</anchorfile>
      <anchor>gabbfb92224c992c0e2ecef6b6e45260f2</anchor>
      <arglist>(char *__hwloc_restrict str, size_t size, size_t nobj, const hwloc_obj_t *__hwloc_restrict objs)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_binding</name>
    <title>Binding</title>
    <filename>a00036.html</filename>
    <member kind="enumeration">
      <name>hwloc_cpubind_policy_t</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>ga9b2de9a34a18edb39fb272adf9c33622</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_CPUBIND_PROCESS</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>gga9b2de9a34a18edb39fb272adf9c33622a2e0dd0128dac6b03408c7dd170477fdc</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_CPUBIND_THREAD</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>gga9b2de9a34a18edb39fb272adf9c33622af1b6bbad00d7b1017b918e3719f4d421</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>HWLOC_CPUBIND_STRICT</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>gga9b2de9a34a18edb39fb272adf9c33622a679a7e0f0c7ee06b123565f90d98e7fa</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_set_cpubind</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>ga42f02baaf7dc0c0f5a6bbeca731fd144</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, int policy)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_get_cpubind</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>ga55427f8da8073ae16d0bab11f8137f1c</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_cpuset_t set, int policy)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_set_proc_cpubind</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>gac349497da8f4f738bad51b2861461dc3</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t set, int policy)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_get_proc_cpubind</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>ga02141a2049739d63a5fa7a172d301f1c</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int policy)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_set_thread_cpubind</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>ga2be36e3ab9c9076ab5cca8fd57ae0dcf</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_const_cpuset_t set, int policy)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_get_thread_cpubind</name>
      <anchorfile>a00036.html</anchorfile>
      <anchor>ga5f265c78ea768372bae8e5d89d628c22</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_cpuset_t set, int policy)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_types</name>
    <title>Object Type Helpers</title>
    <filename>a00037.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int __hwloc_attribute_pure</type>
      <name>hwloc_get_type_or_below_depth</name>
      <anchorfile>a00037.html</anchorfile>
      <anchor>ga0ffafb4c0ae13b9a7541ca820ca34883</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int __hwloc_attribute_pure</type>
      <name>hwloc_get_type_or_above_depth</name>
      <anchorfile>a00037.html</anchorfile>
      <anchor>ga24b9cd5917fcebac6e45ae38d0a6cda4</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_traversal_basic</name>
    <title>Basic Traversal Helpers</title>
    <filename>a00038.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_root_obj</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>ga632edae4a651996895ebde85ea2c1264</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_ancestor_obj_by_depth</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>gafa813c4ff8d610b3e158224a56386b2f</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, unsigned depth, hwloc_obj_t obj)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_ancestor_obj_by_type</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>ga20ade151cb33991b4cd960924a830764</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_type_t type, hwloc_obj_t obj)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_obj_by_depth</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>gab7c1dce3f42ece5bfa621e87cf332418</anchor>
      <arglist>(hwloc_topology_t topology, unsigned depth, hwloc_obj_t prev)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_obj_by_type</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>ga5f08ceb69375341e73563cfe2e77534e</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type, hwloc_obj_t prev)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_pu_obj_by_os_index</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>ga61e00b83e7e0a1a946dc1bb29c49ccba</anchor>
      <arglist>(hwloc_topology_t topology, unsigned os_index)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_child</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>gae5ef1af636849f77714e1584ba78cf9c</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t parent, hwloc_obj_t prev)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_common_ancestor_obj</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>gac574b7b06d2d937002dd538e08dbd554</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj1, hwloc_obj_t obj2)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int __hwloc_attribute_pure</type>
      <name>hwloc_obj_is_in_subtree</name>
      <anchorfile>a00038.html</anchorfile>
      <anchor>ga38d9bd3a7566d0e6b0ab95d652557707</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj, hwloc_obj_t subtree_root)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_find_inside</name>
    <title>Finding Objects Inside a CPU set</title>
    <filename>a00039.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_first_largest_obj_inside_cpuset</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>gabcd5fa81a95fa5335950cae092277d5b</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_get_largest_objs_inside_cpuset</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>gaab04c89623662e63a48ed2cd48eb601c</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, hwloc_obj_t *__hwloc_restrict objs, int max)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_obj_inside_cpuset_by_depth</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>ga8af256c2572f16520f95440b884c1bd6</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, unsigned depth, hwloc_obj_t prev)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_obj_inside_cpuset_by_type</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>ga934e7ecd68b33403e0c0be779d9ed1e6</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, hwloc_obj_type_t type, hwloc_obj_t prev)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_obj_inside_cpuset_by_depth</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>ga20703980008f82379f98f56857611a1a</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, unsigned depth, unsigned idx)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_obj_inside_cpuset_by_type</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>ga50a80a0021e5843d968c3b97aebaad9b</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, hwloc_obj_type_t type, unsigned idx)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline unsigned __hwloc_attribute_pure</type>
      <name>hwloc_get_nbobjs_inside_cpuset_by_depth</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>ga6807db0012369efe19b8d3dcee235493</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, unsigned depth)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int __hwloc_attribute_pure</type>
      <name>hwloc_get_nbobjs_inside_cpuset_by_type</name>
      <anchorfile>a00039.html</anchorfile>
      <anchor>ga72c5bc4317a4c3938e32447b769813a0</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, hwloc_obj_type_t type)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_find_covering</name>
    <title>Finding a single Object covering at least CPU set</title>
    <filename>a00040.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_child_covering_cpuset</name>
      <anchorfile>a00040.html</anchorfile>
      <anchor>gab56b99460194bbcb36016d36d55132a7</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_const_cpuset_t set, hwloc_obj_t parent)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_obj_covering_cpuset</name>
      <anchorfile>a00040.html</anchorfile>
      <anchor>ga2a0de36ea0c3c70fb5f4cba0bb192582</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_find_coverings</name>
    <title>Finding a set of similar Objects covering at least a CPU set</title>
    <filename>a00041.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_obj_covering_cpuset_by_depth</name>
      <anchorfile>a00041.html</anchorfile>
      <anchor>ga2f9a4ec15e9cfae8c21501257a51ce5b</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, unsigned depth, hwloc_obj_t prev)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t</type>
      <name>hwloc_get_next_obj_covering_cpuset_by_type</name>
      <anchorfile>a00041.html</anchorfile>
      <anchor>ga5915ea30f326676b3a4cfff371ce04d1</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set, hwloc_obj_type_t type, hwloc_obj_t prev)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_find_cache</name>
    <title>Cache-specific Finding Helpers</title>
    <filename>a00042.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_cache_covering_cpuset</name>
      <anchorfile>a00042.html</anchorfile>
      <anchor>gae744419648117cbd613a038074aa0627</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t set)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_shared_cache_covering_obj</name>
      <anchorfile>a00042.html</anchorfile>
      <anchor>ga75e961873d4b976ab10bc4739248c96d</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_traversal</name>
    <title>Advanced Traversal Helpers</title>
    <filename>a00043.html</filename>
    <member kind="function">
      <type>HWLOC_DECLSPEC unsigned</type>
      <name>hwloc_get_closest_objs</name>
      <anchorfile>a00043.html</anchorfile>
      <anchor>ga26c2ac4f25b1ed293249c88e232f1bea</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_t src, hwloc_obj_t *__hwloc_restrict objs, unsigned max)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_obj_below_by_type</name>
      <anchorfile>a00043.html</anchorfile>
      <anchor>ga3d32c128aa36b5c9d56f6bf9e70d0e78</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_type_t type1, unsigned idx1, hwloc_obj_type_t type2, unsigned idx2)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure</type>
      <name>hwloc_get_obj_below_array_by_type</name>
      <anchorfile>a00043.html</anchorfile>
      <anchor>ga340bb7021204078c30382ea77d38bde9</anchor>
      <arglist>(hwloc_topology_t topology, int nr, hwloc_obj_type_t *typev, unsigned *idxv)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_binding</name>
    <title>Binding Helpers</title>
    <filename>a00044.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline void</type>
      <name>hwloc_distribute</name>
      <anchorfile>a00044.html</anchorfile>
      <anchor>gaa03a77c4c210a95989ef803ebd9c4524</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_obj_t root, hwloc_cpuset_t *cpuset, unsigned n)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_helper_cpuset</name>
    <title>Cpuset Helpers</title>
    <filename>a00045.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_const_cpuset_t __hwloc_attribute_pure</type>
      <name>hwloc_topology_get_complete_cpuset</name>
      <anchorfile>a00045.html</anchorfile>
      <anchor>ga75f0ac3ac41e9915541c3ae3153a6e26</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_const_cpuset_t __hwloc_attribute_pure</type>
      <name>hwloc_topology_get_topology_cpuset</name>
      <anchorfile>a00045.html</anchorfile>
      <anchor>ga4497338d1cbae6f8a6d68cb14234d5d8</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_const_cpuset_t __hwloc_attribute_pure</type>
      <name>hwloc_topology_get_online_cpuset</name>
      <anchorfile>a00045.html</anchorfile>
      <anchor>gad00abc77f1670049a5b2139471d0c8db</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline hwloc_const_cpuset_t __hwloc_attribute_pure</type>
      <name>hwloc_topology_get_allowed_cpuset</name>
      <anchorfile>a00045.html</anchorfile>
      <anchor>ga95f116c4c0b1ff2c6418c16341fc2e57</anchor>
      <arglist>(hwloc_topology_t topology)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_cpuset</name>
    <title>The Cpuset API</title>
    <filename>a00046.html</filename>
    <member kind="define">
      <type>#define</type>
      <name>hwloc_cpuset_foreach_begin</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga8f896ce703ad1740fdf9ce8ac6361359</anchor>
      <arglist>(cpu, set)</arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>hwloc_cpuset_foreach_end</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gae2974be78a7d7cddbd38cb23fcc6240a</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="typedef">
      <type>struct hwloc_cpuset_s *</type>
      <name>hwloc_cpuset_t</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga7366332f7090f5b54d4b25a0c2c4b411</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>struct hwloc_cpuset_s *</type>
      <name>hwloc_const_cpuset_t</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gad2f7833583d020af31e01554251dbe11</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_cpuset_t</type>
      <name>hwloc_cpuset_alloc</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gaf270165b6a08e8418fcfb68f5793ff7f</anchor>
      <arglist>(void) __hwloc_attribute_malloc</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_free</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gaaac6c1536cdcc35f1a1a3a9ab84da80d</anchor>
      <arglist>(hwloc_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC hwloc_cpuset_t</type>
      <name>hwloc_cpuset_dup</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga468c6e3fd92a9d0db1fb56634a851be3</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_malloc</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_copy</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga27a3b6994bd6f20c1f26d10bdb29ac0b</anchor>
      <arglist>(hwloc_cpuset_t dst, hwloc_const_cpuset_t src)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_snprintf</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga4ed0a2badc6ff03f4d91a8d3c505b3e6</anchor>
      <arglist>(char *__hwloc_restrict buf, size_t buflen, hwloc_const_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_asprintf</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga7a89398cbc58c9095aa094b9aeacbf00</anchor>
      <arglist>(char **strp, hwloc_const_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_from_string</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gab6fb26149e25d4e5719a787ee01bacaa</anchor>
      <arglist>(hwloc_cpuset_t set, const char *__hwloc_restrict string)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_zero</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gacabf3491be3ab41b4ad1ee28f72db89e</anchor>
      <arglist>(hwloc_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_fill</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gacdc29003a0663e9b8b3a9d405a94fb70</anchor>
      <arglist>(hwloc_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_from_ulong</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga81218f1945e8fa25bbbc4e6277019122</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned long mask)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_from_ith_ulong</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gac473267f1aa161c3e3e2a26ef25a477c</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned i, unsigned long mask)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC unsigned long</type>
      <name>hwloc_cpuset_to_ulong</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga5eb912bf1d0572127c3eed1b8a47e6ac</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC unsigned long</type>
      <name>hwloc_cpuset_to_ith_ulong</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga7576f6a70291feafe9e538942c8b7ee5</anchor>
      <arglist>(hwloc_const_cpuset_t set, unsigned i) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_cpu</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga8ee7aa4827fb49af47eac9b66c74fd78</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned cpu)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_all_but_cpu</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga02d86bba61be473bfdceb336c9087736</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned cpu)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_set</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga793d0c31b524337355ddce1c6568a866</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned cpu)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_set_range</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga2f15dc90a98d14db8022f76a38c39727</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned begincpu, unsigned endcpu)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_clr</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga59f2a65f5260581ee642b0a8375be564</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned cpu)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_clr_range</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga214d599ff5b66073460b7ee9a75016a8</anchor>
      <arglist>(hwloc_cpuset_t set, unsigned begincpu, unsigned endcpu)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_isset</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga7236a9cf8be3ded29a912790e35065f7</anchor>
      <arglist>(hwloc_const_cpuset_t set, unsigned cpu) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_iszero</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gac5b8ad0c32e9d14c587eabde188182a9</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_isfull</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gab8703a0f28053bd3981852548e3182d1</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_isequal</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga9534d84820beade1e6155a1e734307a2</anchor>
      <arglist>(hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_intersects</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gad7cbab558a9a80652c3ad0b30d488f04</anchor>
      <arglist>(hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_isincluded</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga135bbe4177fbfe8b14bcbe6aad765801</anchor>
      <arglist>(hwloc_const_cpuset_t sub_set, hwloc_const_cpuset_t super_set) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_or</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga9654f87331e6f33090bed3d326346e85</anchor>
      <arglist>(hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_and</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gacd5a399d475b7d75e71489177650b6df</anchor>
      <arglist>(hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_andnot</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga338fa981505cb2c87e3e9dc543a698b9</anchor>
      <arglist>(hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_xor</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga82f8b81aa98c3a488e21838620da8852</anchor>
      <arglist>(hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_not</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga9494650a8cb93e1dc77590e2393519a5</anchor>
      <arglist>(hwloc_cpuset_t res, hwloc_const_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_first</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gaec614784aab4c1bd4d279fc548f4aa40</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_last</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gaf4fb6d1ca812633f2e5eaa8ae98b1aef</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_next</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga6bf3fd9ea7b0d0fcb5656bab8b68c1bf</anchor>
      <arglist>(hwloc_const_cpuset_t set, unsigned prev_cpu) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC void</type>
      <name>hwloc_cpuset_singlify</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gace7ad3d2a71d9884e7a28311228931af</anchor>
      <arglist>(hwloc_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_compare_first</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga6d54e9fa190351368ea08d02b6b09d32</anchor>
      <arglist>(hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_compare</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>gad3ec83a8f86764d87676a7a48c837d70</anchor>
      <arglist>(hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_cpuset_weight</name>
      <anchorfile>a00046.html</anchorfile>
      <anchor>ga432291e25ca6e91ab689b08cdc26d3fa</anchor>
      <arglist>(hwloc_const_cpuset_t set) __hwloc_attribute_pure</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_glibc_sched</name>
    <title>Helpers for manipulating glibc sched affinity</title>
    <filename>a00047.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_to_glibc_sched_affinity</name>
      <anchorfile>a00047.html</anchorfile>
      <anchor>ga39454e6013441d32e58ef4c4fcba7e4b</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_const_cpuset_t hwlocset, cpu_set_t *schedset, size_t schedsetsize)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_from_glibc_sched_affinity</name>
      <anchorfile>a00047.html</anchorfile>
      <anchor>ga6df504b2f5440b527be05cdad6b1655e</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_cpuset_t hwlocset, const cpu_set_t *schedset, size_t schedsetsize)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_linux</name>
    <title>Linux-only helpers</title>
    <filename>a00048.html</filename>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_linux_parse_cpumap_file</name>
      <anchorfile>a00048.html</anchorfile>
      <anchor>gaeacad897c30dbea284948374ad4b010c</anchor>
      <arglist>(FILE *file, hwloc_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_linux_set_tid_cpubind</name>
      <anchorfile>a00048.html</anchorfile>
      <anchor>gaaaca5d1687053b6c3326b2c165bd6530</anchor>
      <arglist>(hwloc_topology_t topology, pid_t tid, hwloc_const_cpuset_t set)</arglist>
    </member>
    <member kind="function">
      <type>HWLOC_DECLSPEC int</type>
      <name>hwloc_linux_get_tid_cpubind</name>
      <anchorfile>a00048.html</anchorfile>
      <anchor>gaf36a9211a21eb930f59090eb5d460b8e</anchor>
      <arglist>(hwloc_topology_t topology, pid_t tid, hwloc_cpuset_t set)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_linux_libnuma_ulongs</name>
    <title>Helpers for manipulating Linux libnuma unsigned long masks</title>
    <filename>a00049.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_to_linux_libnuma_ulongs</name>
      <anchorfile>a00049.html</anchorfile>
      <anchor>ga018e57a42a780ce2ba2e35ef975d8888</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, unsigned long *mask, unsigned long *maxnode)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_from_linux_libnuma_ulongs</name>
      <anchorfile>a00049.html</anchorfile>
      <anchor>gafa60816dde33d69149497bcf6c7818e0</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_cpuset_t cpuset, const unsigned long *mask, unsigned long maxnode)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_linux_libnuma_bitmask</name>
    <title>Helpers for manipulating Linux libnuma bitmask</title>
    <filename>a00050.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline struct bitmask *__hwloc_attribute_malloc</type>
      <name>hwloc_cpuset_to_linux_libnuma_bitmask</name>
      <anchorfile>a00050.html</anchorfile>
      <anchor>ga067ec565345a346bfd9d721cff5901ae</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_from_linux_libnuma_bitmask</name>
      <anchorfile>a00050.html</anchorfile>
      <anchor>ga47747968f12c2674d2840dfbacce4940</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_cpuset_t cpuset, const struct bitmask *bitmask)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwlocality_linux_libnuma_nodemask</name>
    <title>Helpers for manipulating Linux libnuma nodemask_t</title>
    <filename>a00051.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_to_linux_libnuma_nodemask</name>
      <anchorfile>a00051.html</anchorfile>
      <anchor>ga36feb81315de87ce11d9a5aa2b4c6e6d</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, nodemask_t *nodemask)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_cpuset_from_linux_libnuma_nodemask</name>
      <anchorfile>a00051.html</anchorfile>
      <anchor>gac24c9e4bb2eab3e23b2039559adc9df8</anchor>
      <arglist>(hwloc_topology_t topology, hwloc_cpuset_t cpuset, const nodemask_t *nodemask)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>hwloc_openfabrics</name>
    <title>OpenFabrics-Specific Functions</title>
    <filename>a00052.html</filename>
    <member kind="function" static="yes">
      <type>static __hwloc_inline int</type>
      <name>hwloc_ibv_get_device_cpuset</name>
      <anchorfile>a00052.html</anchorfile>
      <anchor>gaa8ea979ce3a9b8c70ae80bc5716a0fbe</anchor>
      <arglist>(hwloc_topology_t topology __hwloc_attribute_unused, struct ibv_device *ibdev, hwloc_cpuset_t set)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_obj</name>
    <filename>a00011.html</filename>
    <member kind="variable">
      <type>hwloc_obj_type_t</type>
      <name>type</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>acc4f0803f244867e68fe0036800be5de</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>os_index</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a61a7a80a68eaccbaaa28269e678c81a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>char *</type>
      <name>name</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>abb709ec38f2970677e4e57d1d30be96d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj_memory_s</type>
      <name>memory</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a1dc830816716213b5f797e4052487864</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>union hwloc_obj_attr_u *</type>
      <name>attr</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>accd40e29f71f19e88db62ea3df02adc8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>depth</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a9d82690370275d42d652eccdea5d3ee5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>logical_index</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a0d07fb7b8935e137c94d75a3eb492ae9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>signed</type>
      <name>os_level</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a68766f0b1c4d61b5bad87e3b81dacfde</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>next_cousin</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a85a788017457129589318b6c39451acf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>prev_cousin</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>ac715989f55ff5a0eb6be2969ee477ec0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>parent</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>adc494f6aed939992be1c55cca5822900</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>sibling_rank</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>aaa6043eee6f55869933c1d974efd9acd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>next_sibling</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a7f2343ed476fe4942e6fffd4cade1b40</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>prev_sibling</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a7b89e8c189876c0158a9282aaaf17f50</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>arity</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>aac3f6da35c9b57599909a44ce2b716c1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj **</type>
      <name>children</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a04d05403da37bfe17cd63b7c7dd07b1f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>first_child</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>af51d08a0a79dba517c06c5afedc8d2dc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj *</type>
      <name>last_child</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a84bd65634dbc55f4158b74443a9bd04f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>void *</type>
      <name>userdata</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a76fd3ac94401cf32dfccc3a3a8de68a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>cpuset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a67925e0f2c47f50408fbdb9bddd0790f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>complete_cpuset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a91788a9da687beb7224cc1fd7b75208c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>online_cpuset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a8842d56c2975380f327ea401c5f53564</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>allowed_cpuset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>afa3c59a6dd3da8ffa48710780a1bfb34</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>nodeset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a79982ede313c2190505fc5e3714a16fb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>complete_nodeset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>adc473a52c071d7fd49e659ac90467a0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>hwloc_cpuset_t</type>
      <name>allowed_nodeset</name>
      <anchorfile>a00011.html</anchorfile>
      <anchor>a9c1af614e0978a65ce309f921a822c8b</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="union">
    <name>hwloc_obj_attr_u</name>
    <filename>a00012.html</filename>
    <class kind="struct">hwloc_obj_attr_u::hwloc_cache_attr_s</class>
    <class kind="struct">hwloc_obj_attr_u::hwloc_group_attr_s</class>
    <class kind="struct">hwloc_obj_attr_u::hwloc_machine_attr_s</class>
    <member kind="variable">
      <type>struct hwloc_obj_attr_u::hwloc_cache_attr_s</type>
      <name>cache</name>
      <anchorfile>a00012.html</anchorfile>
      <anchor>ab5a8ae3bf490e6b1071fea53f7382836</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj_attr_u::hwloc_machine_attr_s</type>
      <name>machine</name>
      <anchorfile>a00012.html</anchorfile>
      <anchor>a5b42966df7c5bfdc36891e414cc31607</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj_attr_u::hwloc_group_attr_s</type>
      <name>group</name>
      <anchorfile>a00012.html</anchorfile>
      <anchor>ae4ba157cc313e2cdd9a82f1c1df7aaa6</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_obj_attr_u::hwloc_cache_attr_s</name>
    <filename>a00008.html</filename>
    <member kind="variable">
      <type>uint64_t</type>
      <name>size</name>
      <anchorfile>a00008.html</anchorfile>
      <anchor>a3c68235220554308f89768f281ad1e62</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>depth</name>
      <anchorfile>a00008.html</anchorfile>
      <anchor>a5c8f7f39193736c2187ed626940835d5</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_obj_attr_u::hwloc_group_attr_s</name>
    <filename>a00009.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>depth</name>
      <anchorfile>a00009.html</anchorfile>
      <anchor>ad914eac61c77481e1b7037877bcc5579</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_obj_attr_u::hwloc_machine_attr_s</name>
    <filename>a00010.html</filename>
    <member kind="variable">
      <type>char *</type>
      <name>dmi_board_vendor</name>
      <anchorfile>a00010.html</anchorfile>
      <anchor>aae85be0b4ebb86501718c4b460df5167</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>char *</type>
      <name>dmi_board_name</name>
      <anchorfile>a00010.html</anchorfile>
      <anchor>a8b99af84fd38753a91c861f0e856b461</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_obj_memory_s</name>
    <filename>a00014.html</filename>
    <class kind="struct">hwloc_obj_memory_s::hwloc_obj_memory_page_type_s</class>
    <member kind="variable">
      <type>uint64_t</type>
      <name>total_memory</name>
      <anchorfile>a00014.html</anchorfile>
      <anchor>a68c3323d2d0a248d1b7fec7af44bebe3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>uint64_t</type>
      <name>local_memory</name>
      <anchorfile>a00014.html</anchorfile>
      <anchor>a27043a3150660f44ed84916c2d0d7e0e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>page_types_len</name>
      <anchorfile>a00014.html</anchorfile>
      <anchor>a208c27f4491077d7fb9ba5db8b29cb57</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_obj_memory_s::hwloc_obj_memory_page_type_s *</type>
      <name>page_types</name>
      <anchorfile>a00014.html</anchorfile>
      <anchor>a865eba7b12b986d72dbe7a2cfd97c50d</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_obj_memory_s::hwloc_obj_memory_page_type_s</name>
    <filename>a00013.html</filename>
    <member kind="variable">
      <type>uint64_t</type>
      <name>size</name>
      <anchorfile>a00013.html</anchorfile>
      <anchor>af0619463fb5d10052b7fe3495a66d74b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>uint64_t</type>
      <name>count</name>
      <anchorfile>a00013.html</anchorfile>
      <anchor>ab5d01db7b26177a6b5361107cad152c3</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_topology_cpubind_support</name>
    <filename>a00015.html</filename>
    <member kind="variable">
      <type>unsigned char</type>
      <name>set_thisproc_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>a9403d51657a4d546b3ea9553a2973a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>get_thisproc_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>a77a09ddd78ee3e9ff5f532a6ac74f7eb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>set_proc_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>aa166223d1c2a6de7256ab2d8b675a87e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>get_proc_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>aae705bc447adc163ead377362c4dfe9f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>set_thisthread_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>a57a89a4b5f1f74fa6cfe176f1e8b0798</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>get_thisthread_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>a80d762e532d677dff262d83cc7bb1c60</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>set_thread_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>a46fba33e307909ce256624687799dd6d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned char</type>
      <name>get_thread_cpubind</name>
      <anchorfile>a00015.html</anchorfile>
      <anchor>a8dd4d8531ed2eebdce1507e7d104154e</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_topology_discovery_support</name>
    <filename>a00016.html</filename>
    <member kind="variable">
      <type>unsigned char</type>
      <name>pu</name>
      <anchorfile>a00016.html</anchorfile>
      <anchor>ad7bb4ecf7a82f5a04fc632e9592ad3ab</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>hwloc_topology_support</name>
    <filename>a00017.html</filename>
    <member kind="variable">
      <type>struct hwloc_topology_discovery_support *</type>
      <name>discovery</name>
      <anchorfile>a00017.html</anchorfile>
      <anchor>aea3fbd7653d987d81f848636c420504d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>struct hwloc_topology_cpubind_support *</type>
      <name>cpubind</name>
      <anchorfile>a00017.html</anchorfile>
      <anchor>adef2bb91f74c3e70a2a071393caf5f56</anchor>
      <arglist></arglist>
    </member>
  </compound>
</tagfile>
