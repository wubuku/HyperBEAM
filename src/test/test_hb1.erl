-module(test_hb1).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
 
%% Run with: rebar3 eunit --module=test_hb1
 
type_coercion_test() ->
    %% Integer conversion
    ?assertEqual(42, hb_util:int(<<"42">>)),
    ?assertEqual(42, hb_util:int("42")),
    ?assertEqual(-123, hb_util:int(<<"-123">>)),
    ?debugFmt("Integer conversion: OK", []),
    
    %% Binary conversion
    ?assertEqual(<<"hello">>, hb_util:bin(hello)),
    ?assertEqual(<<"42">>, hb_util:bin(42)),
    ?debugFmt("Binary conversion: OK", []),
    
    %% List conversion
    ?assertEqual("hello", hb_util:list(<<"hello">>)),
    ?debugFmt("List conversion: OK", []).
 
encoding_test() ->
    %% Generate random data
    Data = crypto:strong_rand_bytes(32),
    
    %% Encode and decode
    Encoded = hb_util:encode(Data),
    ?debugFmt("Encoded 32 bytes to ~p chars", [byte_size(Encoded)]),
    
    Decoded = hb_util:decode(Encoded),
    ?assertEqual(Data, Decoded),
    ?debugFmt("Roundtrip encoding: OK", []),
    
    %% URL-safe (no + or /)
    ?assertEqual(nomatch, binary:match(Encoded, <<"+">>)),
    ?assertEqual(nomatch, binary:match(Encoded, <<"/">>)),
    ?debugFmt("URL-safe encoding verified", []).
 
json_test() ->
    %% Encode map
    Term = #{<<"name">> => <<"Alice">>, <<"age">> => 30},
    JSON = hb_json:encode(Term),
    ?debugFmt("Encoded JSON: ~s", [JSON]),
    
    %% Decode back
    Decoded = hb_json:decode(JSON),
    ?assertEqual(Term, Decoded),
    ?debugFmt("JSON roundtrip: OK", []),
    
    %% Nested structures
    Complex = #{
        <<"user">> => #{
            <<"name">> => <<"Bob">>,
            <<"tags">> => [<<"admin">>, <<"active">>]
        }
    },
    ComplexJSON = hb_json:encode(Complex),
    ?assertEqual(Complex, hb_json:decode(ComplexJSON)),
    ?debugFmt("Nested JSON: OK", []).
 
hashing_test() ->
    %% SHA-256 produces 32 bytes
    Hash = hb_crypto:sha256(<<"hello world">>),
    ?assertEqual(32, byte_size(Hash)),
    ?debugFmt("SHA-256 hash size: 32 bytes", []),
    
    %% Deterministic
    Hash1 = hb_crypto:sha256(<<"test">>),
    Hash2 = hb_crypto:sha256(<<"test">>),
    ?assertEqual(Hash1, Hash2),
    ?debugFmt("SHA-256 deterministic: OK", []),
    
    %% Hash chaining
    ID1 = <<1:256>>,
    ID2 = <<2:256>>,
    Chain = hb_crypto:sha256_chain(ID1, ID2),
    ?assertEqual(32, byte_size(Chain)),
    ?debugFmt("Hash chain: OK", []),
    
    %% Order matters in chaining
    Chain1 = hb_crypto:sha256_chain(ID1, ID2),
    Chain2 = hb_crypto:sha256_chain(ID2, ID1),
    ?assertNotEqual(Chain1, Chain2),
    ?debugFmt("Chain order dependency verified", []).
 
accumulation_test() ->
    %% Accumulate two IDs
    ID1 = <<1:256>>,
    ID2 = <<2:256>>,
    Result = hb_crypto:accumulate(ID1, ID2),
    
    <<ResultInt:256>> = Result,
    ?assertEqual(3, ResultInt),
    ?debugFmt("Accumulate 1 + 2 = 3: OK", []),
    
    %% Order independent
    Acc1 = hb_crypto:accumulate(ID1, ID2),
    Acc2 = hb_crypto:accumulate(ID2, ID1),
    ?assertEqual(Acc1, Acc2),
    ?debugFmt("Accumulation order-independent: OK", []),
    
    %% Accumulate list
    IDs = [<<1:256>>, <<2:256>>, <<3:256>>],
    ListResult = hb_crypto:accumulate(IDs),
    <<ListInt:256>> = ListResult,
    ?assertEqual(6, ListInt),
    ?debugFmt("Accumulate list [1,2,3] = 6: OK", []).
 
escape_test() ->
    %% Percent encoding
    Encoded = hb_escape:encode(<<"Hello World!">>),
    ?debugFmt("Encoded: ~s", [Encoded]),
    
    Decoded = hb_escape:decode(Encoded),
    ?assertEqual(<<"Hello World!">>, Decoded),
    ?debugFmt("Escape roundtrip: OK", []),
    
    %% Lowercase preserved
    ?assertEqual(<<"hello">>, hb_escape:encode(<<"hello">>)),
    ?debugFmt("Lowercase preserved: OK", []).
 
complete_workflow_test() ->
    ?debugFmt("=== Complete Workflow Test ===", []),
    
    %% 1. Create some data
    Data = #{
        <<"type">> => <<"message">>,
        <<"content">> => <<"Hello, HyperBEAM!">>,
        <<"timestamp">> => 1234567890
    },
    ?debugFmt("1. Created data structure", []),
    
    %% 2. Serialize to JSON
    JSON = hb_json:encode(Data),
    ?debugFmt("2. Serialized to JSON: ~s", [JSON]),
    
    %% 3. Hash the content
    Hash = hb_crypto:sha256(JSON),
    HashHex = hb_util:to_hex(Hash),
    ?debugFmt("3. SHA-256 hash: ~s", [HashHex]),
    
    %% 4. Encode hash for URLs
    HashEncoded = hb_util:encode(Hash),
    ?debugFmt("4. Base64url encoded: ~s", [HashEncoded]),
    
    %% 5. Verify roundtrip
    HashDecoded = hb_util:decode(HashEncoded),
    ?assertEqual(Hash, HashDecoded),
    ?debugFmt("5. Verified roundtrip encoding", []),
    
    %% 6. Parse JSON back
    Recovered = hb_json:decode(JSON),
    ?assertEqual(Data, Recovered),
    ?debugFmt("6. Verified JSON roundtrip", []),
    
    ?debugFmt("=== All tests passed! ===", []).