<?php
/**
 * User: DongPeng
 * Date: 08/05/2014
 * Time: 9:23 a.m.
 */
if (!isset($argv[1])) {
    exit("usage: > php proto_gen.php xxx");
}
$module_name = $argv[1];

define('BASE_DIR', dirname(__DIR__));
$proto_file_name = BASE_DIR . "/proto/$module_name.proto";
if (!file_exists($proto_file_name)) {
    exit();
}
$proto_str_array = file($proto_file_name);
//$proto = '// protobuf-like syntax
//
//message privilege 1001 {
//	required int16 id = 0;
//	required int32 role_id =0; //%
//	required string name = ""; %%
//	repeated kv_pair data = [];
//}
//
//message kv_pair 1002 {
//	required int16 key = 0;
//	required int32 val =0;
//}
//
//';
//$proto_str_array = explode("\n", $proto);

$base_types = array(
    'int16','int8','int32','int64','string'
);

//xx.hrl 协议id对应关系 message_id => message_name
$message_rec = array();
//xx_map.erl 主体
$messages  = array();
//xx_rec.erl 参数默认值
$message_defaults= array();

$message_start = false;
$m_name = '';
foreach ($proto_str_array as $_row) {
    $_row = trim($_row);
    if (empty($_row) || strpos('//', $_row)) {
        continue;
    }
    
    //用空格分解关键字, 并去掉多余空格
    $key_words = array();
    $key_words_explode = explode(' ', $_row);
    if (!empty($key_words_explode)) foreach($key_words_explode as $_k) {
        $_k = trim($_k);
        if ($_k !='') {
            $key_words[] = $_k;
        }
    }
    
    if (!empty($key_words)) {
        if ($key_words[0]== 'message') {
            $message_start = true;
            $m_name = $key_words[1];
            $m_id = $key_words[2];
            $message_rec[$m_name] = $m_id;
            $message_defaults[$m_name] = array();
        }else if($key_words[0]== 'required'  || $key_words[0]== 'repeated') {
            $type = $key_words[1];
            if (in_array($type, $base_types)) {
                $messages[$m_name][]    =   $type;
            }else {
                $messages[$m_name][]    =   "{list, $type}";
            }
            unset($key_words[0]);
            unset($key_words[1]);
            $default_value = implode(' ', $key_words);
            $default_value = substr($default_value, 0, strpos($default_value, ';'));
            $message_defaults[$m_name][] = $default_value;
        }else if($key_words[0]== 'required') {

        }else if($key_words[0]== '}') {
            $message_start = false;
        }
    }
}

$file_header = 
'%% -------------------------------------------------------------------
%%
%% Sep (Simple erlang protobuf) auto-generation
%%
%% Copyright (c) 2014 Dong Peng (pengdong1704@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
';
$param_map = array();
$src_map_erl = $file_header;
$src_map_erl .= "-module({$module_name}_map). \r\n\r\n";
$src_map_erl .= "-export([get_proto_map/1]). \r\n\r\n";
foreach($messages as $m_name=> $params) {
    $param_map[$m_name] =   implode(",", $params);
    $src_map_erl .= "get_proto_map($m_name) ->
	{{$message_rec[$m_name]}, [{$param_map[$m_name]}]}; \r\n";
}
$src_map_erl .= "get_proto_map(_) ->
	[].
";
// echo $src_map_erl;

$src_hrl = $file_header;
foreach($message_defaults as $m_name=> $params) {
    $src_hrl .= "%% metadata: [{$param_map[$m_name]}].\r\n";
    $default_str = implode(', ', $params);
    $src_hrl .= "-record($m_name, { $default_str }). \r\n\r\n";
}
// echo $src_hrl;

$src_rec_erl = $file_header;
$src_rec_erl .= "-module(sep_rec). \r\n\r\n";
$src_rec_erl .= "-export([get_proto_rec/1]). \r\n\r\n";
foreach($message_rec as $m_name=> $m_id) {
    $src_rec_erl .= "get_proto_rec($m_id) ->
    $m_name; \r\n";
}
$src_rec_erl .=     "get_proto_rec(_) ->
    [].";
// echo $src_rec_erl;

echo "\r\n ---------------\r\n  \r\n";
if(file_put_contents(BASE_DIR . "/include/$module_name.hrl",$src_hrl )) {
    echo "output: $module_name.hrl success \r\n";
}else {
    echo "output: $module_name.hrl failed \r\n";
}

if(file_put_contents(BASE_DIR . "/src/{$module_name}_rec.erl",$src_rec_erl )) {
    echo "output: {$module_name}_rec.erl success \r\n";
}else {
    echo "output: {$module_name}_rec.erl failed \r\n";
}

if(file_put_contents(BASE_DIR . "/src/{$module_name}_map.erl",$src_map_erl )) {
    echo "output: {$module_name}_map.erl success \r\n";
}else {
    echo "output: {$module_name}_map.erl failed \r\n";
}

echo "\r\n \r\n finished \r\n";
