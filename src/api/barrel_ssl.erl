%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_ssl).

-export([options/1]).
-include_lib("public_key/include/public_key.hrl").

options(Opts) ->
  CertFile = proplists:get_value(certfile, Opts),
  KeyFile = proplists:get_value(keyfile, Opts),

  if
    CertFile /= undefined, KeyFile /= undefined -> ok;
    true ->
      lager:error("SSL enabled but PEM certificates are missing.", []),
      erlang:error({error, missing_certs})
  end,

  Password = case proplists:get_value(password, Opts) of
               undefined -> [];
               P -> [{password, P}]
             end,

  case proplists:get_value("verify_ssl_certificates", Opts, false) of
    true ->
      CaCertFile = proplists:get_value(cacertfile, Opts),
      CertFile = proplists:get_value(certfile, Opts),
      KeyFile = proplists:get_value(keyfile, Opts),
      Versions = proplists:get_value(tls_protocols, Opts, ['tlsv1.2']),
      HonorCipherOrder = proplists:get_value(honor_cipher_order, Opts, false),
      Ciphers = ciphersuite_transform(proplists:get_value(support_elliptic_curve, Opts, true),
        proplists:get_value(ciphers, Opts, [])),
      CheckCRL = proplists:get_value(check_clr, Opts, false),

      CACerts = load_certs(CaCertFile),

      [{certfile, CertFile},
        {keyfile, KeyFile},
        {cacerts, CACerts},
        {ciphers, Ciphers},
        {versions, Versions},
        %% force peer validation, even though
        %% we don't care if the peer doesn't
        %% send a certificate
        {verify, verify_peer},
        {reuse_sessions, false} %% required!
      ] ++ Password ++
      %% conditionally include the honor cipher order, don't pass it if it
      %% disabled because it will crash any
      %% OTP installs that lack the patch to
      %% implement honor_cipher_order
        [{honor_cipher_order, true} || HonorCipherOrder ] ++
        %% if we're validating CRLs, define a
        %% verify_fun for them.
        [{verify_fun, {fun verify_fun/3, {CACerts, []}}} || CheckCRL ];
    false ->
      [{certfile, CertFile}, {keyfile, KeyFile}] ++ Password
  end.


%% internal functions

ciphersuite_transform(SupportEC, []) ->
    unbroken_cipher_suites(all_ciphers(SupportEC));
ciphersuite_transform(_, CiphersString) when is_list(CiphersString) ->
    Ciphers = string:tokens(CiphersString, ":"),
    unbroken_cipher_suites(ciphersuite_transform_(Ciphers, [])).

-spec ciphersuite_transform_([string()],
                             [{atom(), atom(), atom()}]) ->
    [{atom(), atom(), atom()}].
ciphersuite_transform_([CipherString|Rest], Acc) ->
    Cipher = string:tokens(CipherString, "-"),
    case cipher_ex_transform(Cipher) of
        {ok, CipherSuite} ->
            ciphersuite_transform_(Rest, [CipherSuite|Acc]);
        {error, Reason} ->
            lager:error("error parsing ciphersuite ~p, ~p~n",
                        [CipherString, Reason]),
            ciphersuite_transform_(Rest, Acc)
    end;
ciphersuite_transform_([], Acc) -> Acc.

-spec cipher_ex_transform([string()]) ->
    {ok, {atom(), atom(), atom()}} |
    {error, unknown_keyexchange |
     unknown_cipher | unknown_cipher}.
cipher_ex_transform(["ECDH", "ANON"|Rest]) ->
    cipher_ci_transform(ecdh_anon, Rest);
cipher_ex_transform(["ECDH", "ECDSA"|Rest]) ->
    cipher_ci_transform(ecdh_ecdsa, Rest);
cipher_ex_transform(["ECDHE", "ECDSA"|Rest]) ->
    cipher_ci_transform(ecdhe_ecdsa, Rest);
cipher_ex_transform(["ECDH", "RSA"|Rest]) ->
    cipher_ci_transform(ecdh_rsa, Rest);
cipher_ex_transform(["ECDHE", "RSA"|Rest]) ->
    cipher_ci_transform(ecdhe_rsa, Rest);
cipher_ex_transform(["DHE", "DSS"|Rest]) -> cipher_ci_transform(dhe_dss, Rest);
cipher_ex_transform(["DHE", "RSA"|Rest]) -> cipher_ci_transform(dhe_rsa, Rest);
cipher_ex_transform(["DH", "ANON"|Rest]) -> cipher_ci_transform(dh_anon, Rest);
cipher_ex_transform(["DHE", "PSK"|Rest]) -> cipher_ci_transform(dhe_psk, Rest);
cipher_ex_transform(["RSA", "PSK"|Rest]) -> cipher_ci_transform(rsa_psk, Rest);
cipher_ex_transform(["SRP", "ANON"|Rest]) ->
    cipher_ci_transform(srp_anon, Rest);
cipher_ex_transform(["SRP", "DSS"|Rest]) -> cipher_ci_transform(srp_dss, Rest);
cipher_ex_transform(["SRP", "RSA"|Rest]) -> cipher_ci_transform(srp_rsa, Rest);
cipher_ex_transform(["RSA"|Rest]) -> cipher_ci_transform(rsa, Rest);
cipher_ex_transform(["PSK"|Rest]) -> cipher_ci_transform(psk, Rest);
cipher_ex_transform([_|Rest]) -> cipher_ex_transform(Rest);
cipher_ex_transform([]) -> {error, unknown_keyexchange}.

-spec cipher_ci_transform(atom(), [string()]) ->
    {ok, {atom(), atom(), atom()}} |
    {error, unknown_hash | unknown_cipher}.
cipher_ci_transform(KeyEx, ["3DES", "EDE", "CBC"|Rest]) ->
    cipher_hash_transform(KeyEx, '3des_ede_cbc', Rest);
cipher_ci_transform(KeyEx, ["AES128", "CBC"|Rest]) ->
    cipher_hash_transform(KeyEx, aes_128_cbc, Rest);
cipher_ci_transform(KeyEx, ["AES256", "CBC"|Rest]) ->
    cipher_hash_transform(KeyEx, aes_256_cbc, Rest);
cipher_ci_transform(KeyEx, ["RC4", "128"|Rest]) ->
    cipher_hash_transform(KeyEx, rc4_128, Rest);
cipher_ci_transform(KeyEx, ["DES", "CBC"|Rest]) ->
    cipher_hash_transform(KeyEx, des_cbc, Rest);
cipher_ci_transform(KeyEx, [_|Rest]) ->
    cipher_ci_transform(KeyEx, Rest);
cipher_ci_transform(_, []) -> {error, unknown_cipher}.

-spec cipher_hash_transform(atom(), atom(), [string()]) ->
    {ok, {atom(), atom(), atom()}} |
    {error, unknown_hash}.
cipher_hash_transform(KeyEx, Cipher, ["MD5"]) -> {ok, {KeyEx, Cipher, md5}};
cipher_hash_transform(KeyEx, Cipher, ["SHA"]) -> {ok, {KeyEx, Cipher, sha}};
cipher_hash_transform(KeyEx, Cipher, ["SHA256"]) ->
    {ok, {KeyEx, Cipher, sha256}};
cipher_hash_transform(KeyEx, Cipher, ["SHA384"]) ->
    {ok, {KeyEx, Cipher, sha384}};
cipher_hash_transform(KeyEx, Cipher, [_|Rest]) ->
    cipher_hash_transform(KeyEx, Cipher, Rest);
cipher_hash_transform(_, _, []) -> {error, unknown_hash}.

-spec all_ciphers(boolean()) -> [{atom(), atom(), atom()}].
all_ciphers(UseEc) ->
    ECExchanges = [ecdh_anon, ecdh_ecdsa, ecdhe_ecdsa, ecdh_rsa, ecdhe_rsa],
    lists:foldl(fun({Ex, _, _} = CS, Acc) ->
                        case UseEc of
                            true -> [CS|Acc];
                            false ->
                                case lists:member(Ex, ECExchanges) of
                                    true -> Acc;
                                    false -> [CS|Acc]
                                end
                        end;
                   (_, Acc) -> Acc
                end, [], ssl:cipher_suites()).

-spec unbroken_cipher_suites([atom()]) -> [{atom(), atom(), atom()}].
unbroken_cipher_suites(CipherSuites) ->
    %% from ranch
    case proplists:get_value(ssl_app, ssl:versions()) of
        Version when Version =:= "5.3"; Version =:= "5.3.1" ->
            lists:filter(fun(Suite) ->
                                 string:left(
                                   atom_to_list(element(1,
                                                        Suite)), 4) =/= "ecdh"
                         end, CipherSuites);
        _ ->
            CipherSuites
    end.


% @doc Validator function for SSL negotiation.
%%
verify_fun(Cert, valid_peer, State) ->
  lager:debug("validing peer ~p with ~p intermediate certs",
              [get_common_name(Cert),
               length(element(2, State))]),
  %% peer certificate validated, now check the CRL
  Res = (catch check_crl(Cert, State)),
  lager:debug("CRL validate result for ~p: ~p",
              [get_common_name(Cert), Res]),
  {Res, State};
verify_fun(Cert, valid, {TrustedCAs, IntermediateCerts}=State) ->
  case public_key:pkix_is_self_signed(Cert) of
    true ->
      %% this is a root cert, no CRL
      {valid, {TrustedCAs, [Cert|IntermediateCerts]}};
    false ->
      %% check is valid CA certificate, add to the list of
      %% intermediates
      Res = (catch check_crl(Cert, State)),
      lager:debug("CRL intermediate CA validate result for ~p: ~p",
                  [get_common_name(Cert), Res]),
      {Res, {TrustedCAs, [Cert|IntermediateCerts]}}
  end;
verify_fun(_Cert, {bad_cert, _} = Reason, _UserState) ->
  {fail, Reason};
verify_fun(_Cert, {extension, _}, UserState) ->
  {unknown, UserState}.

%% @doc Given a certificate, find CRL distribution points for the given
%%      certificate, fetch, and attempt to validate each CRL through
%%      issuer_function/4.
%%
check_crl(Cert, State) ->
  %% pull the CRL distribution point(s) out of the certificate, if any
  case pubkey_cert:select_extension(?'id-ce-cRLDistributionPoints',
                                    pubkey_cert:extensions_list(Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.extensions)) of
    undefined ->
      lager:debug("no CRL distribution points for ~p",
                  [get_common_name(Cert)]),
      %% fail; we can't validate if there's no CRL
      no_crl;
    CRLExtension ->
      CRLDistPoints = CRLExtension#'Extension'.extnValue,
      DPointsAndCRLs = lists:foldl(fun(Point, Acc) ->
                                       %% try to read the CRL over http or from a
                                       %% local file
                                       case fetch_point(Point) of
                                         not_available ->
                                           Acc;
                                         Res ->
                                           [{Point, Res} | Acc]
                                       end
                                   end, [], CRLDistPoints),
      public_key:pkix_crls_validate(Cert,
                                    DPointsAndCRLs,
                                    [{issuer_fun,
                                      {fun issuer_function/4, State}}])
  end.

%% @doc Given a list of distribution points for CRLs, certificates and
%%      both trusted and intermediary certificates, attempt to build and
%%      authority chain back via build_chain to verify that it is valid.
%%
issuer_function(_DP, CRL, _Issuer, {TrustedCAs, IntermediateCerts}) ->
  %% XXX the 'Issuer' we get passed here is the AuthorityKeyIdentifier,
  %% which we are not currently smart enough to understand
  %% Read the CA certs out of the file
  Certs = [public_key:pkix_decode_cert(DER, otp) || DER <- TrustedCAs],
  %% get the real issuer out of the CRL
  Issuer = public_key:pkix_normalize_name(
             pubkey_cert_records:transform(
               CRL#'CertificateList'.tbsCertList#'TBSCertList'.issuer, decode)),
  %% assume certificates are ordered from root to tip
  case find_issuer(Issuer, IntermediateCerts ++ Certs) of
    undefined ->
      lager:debug("unable to find certificate matching CRL issuer ~p",
                  [Issuer]),
      error;
    IssuerCert ->
      case build_chain({public_key:pkix_encode('OTPCertificate',
                                               IssuerCert,
                                               otp),
                        IssuerCert}, IntermediateCerts, Certs, []) of
        undefined ->
          error;
        {OTPCert, Path} ->
          {ok, OTPCert, Path}
      end
  end.

%% @doc Attempt to build authority chain back using intermediary
%%      certificates, falling back on trusted certificates if the
%%      intermediary chain of certificates does not fully extend to the
%%      root.
%%
%%      Returns: {RootCA :: #OTPCertificate{}, Chain :: [der_encoded()]}
%%
build_chain({DER, Cert}, IntCerts, TrustedCerts, Acc) ->
  %% check if this cert is self-signed, if it is, we've reached the
  %% root of the chain
  Issuer = public_key:pkix_normalize_name(
             Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer),
  Subject = public_key:pkix_normalize_name(
              Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject),
  case Issuer == Subject of
    true ->
      case find_issuer(Issuer, TrustedCerts) of
        undefined ->
          undefined;
        TrustedCert ->
          %% return the cert from the trusted list, to prevent
          %% issuer spoofing
          {TrustedCert,
           [public_key:pkix_encode(
              'OTPCertificate', TrustedCert, otp)|Acc]}
      end;
    false ->
      Match = lists:foldl(
                fun(C, undefined) ->
                    S = public_key:pkix_normalize_name(C#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject),
                    %% compare the subject to the current issuer
                    case Issuer == S of
                      true ->
                        %% we've found our man
                        {public_key:pkix_encode('OTPCertificate', C, otp), C};
                      false ->
                        undefined
                    end;
                   (_E, A) ->
                    %% already matched
                    A
                end, undefined, IntCerts),
      case Match of
        undefined when IntCerts /= TrustedCerts ->
          %% continue the chain by using the trusted CAs
          lager:debug("Ran out of intermediate certs, switching to trusted certs~n"),
          build_chain({DER, Cert}, TrustedCerts, TrustedCerts, Acc);
        undefined ->
          lager:debug("Can't construct chain of trust beyond ~p",
                      [get_common_name(Cert)]),
          %% can't find the current cert's issuer
          undefined;
        Match ->
          build_chain(Match, IntCerts, TrustedCerts, [DER|Acc])
      end
  end.

%% @doc Given a certificate and a list of trusted or intermediary
%%      certificates, attempt to find a match in the list or bail with
%%      undefined.
find_issuer(Issuer, Certs) ->
  lists:foldl(
    fun(OTPCert, undefined) ->
        %% check if this certificate matches the issuer
        Normal = public_key:pkix_normalize_name(
                   OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject),
        case Normal == Issuer of
          true ->
            OTPCert;
          false ->
            undefined
        end;
       (_E, Acc) ->
        %% already found a match
        Acc
    end, undefined, Certs).

%% @doc Find distribution points for a given CRL and then attempt to
%%      fetch the CRL from the first available.
fetch_point(#'DistributionPoint'{distributionPoint={fullName, Names}}) ->
  Decoded = [{NameType,
              pubkey_cert_records:transform(Name, decode)}
             || {NameType, Name} <- Names],
  fetch(Decoded).

%% @doc Given a list of locations to retrieve a CRL from, attempt to
%%      retrieve either from a file or http resource and bail as soon as
%%      it can be found.
%%
%%      Currently, only hand a armored PEM or DER encoded file, with
%%      defaulting to DER.
%%
fetch([]) ->
  not_available;
fetch([{uniformResourceIdentifier, "file://"++_File}|Rest]) ->
  lager:debug("fetching CRLs from file URIs is not supported"),
  fetch(Rest);
fetch([{uniformResourceIdentifier, "http"++_=URL}|Rest]) ->
  lager:debug("getting CRL from ~p~n", [URL]),
  _ = inets:start(),
  case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
    {ok, {_Status, _Headers, Body}} ->
      case Body of
        <<"-----BEGIN", _/binary>> ->
          [{'CertificateList',
            DER, _}=CertList] = public_key:pem_decode(Body),
          {DER, public_key:pem_entry_decode(CertList)};
        _ ->
          %% assume DER encoded
          CertList = public_key:pem_entry_decode(
                       {'CertificateList', Body, not_encrypted}),
          {Body, CertList}
      end;
    {error, _Reason} ->
      lager:debug("failed to get CRL ~p~n", [_Reason]),
      fetch(Rest)
  end;
fetch([Loc|Rest]) ->
  %% unsupported CRL location
  lager:debug("unable to fetch CRL from unsupported location ~p",
              [Loc]),
  fetch(Rest).

%% get the common name attribute out of an OTPCertificate record
get_common_name(OTPCert) ->
  %% You'd think there'd be an easier way than this giant mess, but I
  %% couldn't find one.
  {rdnSequence, Subject} = OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
  [Att] = [Attribute#'AttributeTypeAndValue'.value || [Attribute] <- Subject,
                                                      Attribute#'AttributeTypeAndValue'.type == ?'id-at-commonName'],
  case Att of
    {printableString, Str} -> Str;
    {utf8String, Bin} -> binary_to_list(Bin)
  end.

load_certs(undefined) ->
  undefined;
load_certs(CertDirOrFile) ->
  case filelib:is_regular(CertDirOrFile) of
    true ->
      load_cert(CertDirOrFile);
    _ ->
      case file:list_dir(CertDirOrFile) of
        {ok, Certs} ->
          load_certs(lists:map(fun(Cert) ->
                                   filename:join(CertDirOrFile, Cert)
                               end, Certs), []);
        {error, _} ->
          undefined
      end
  end.

load_certs([], Acc) ->
  lager:debug("Successfully loaded ~p CA certificates", [length(Acc)]),
  Acc;
load_certs([Cert|Certs], Acc) ->
  case filelib:is_dir(Cert) of
    true ->
      load_certs(Certs, Acc);
    _ ->
      lager:debug("Loading certificate ~p", [Cert]),
      load_certs(Certs, load_cert(Cert) ++ Acc)
  end.

load_cert(Cert) ->
  {ok, Bin} = file:read_file(Cert),
  case filename:extension(Cert) of
    ".der" ->
      %% no decoding necessary
      [Bin];
    _ ->
      %% assume PEM otherwise
      Contents = public_key:pem_decode(Bin),
      [DER || {Type, DER, Cipher} <- Contents,
              Type == 'Certificate', Cipher == 'not_encrypted']
  end.
