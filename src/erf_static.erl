%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License

%% @doc <code>erf</code>'s static files handler utility module.
-module(erf_static).

%%% EXTERNAL EXPORTS
-export([
    mime_type/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec mime_type(Extension) -> MimeType when
    Extension :: binary(),
    MimeType :: binary().
%% @doc Returns the MIME type for the given file extension.
%% https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
mime_type(<<".html">>) ->
    <<"text/html">>;
mime_type(<<".css">>) ->
    <<"text/css">>;
mime_type(<<".js">>) ->
    <<"text/javascript">>;
mime_type(<<".ttf">>) ->
    <<"font/ttf">>;
mime_type(<<".svg">>) ->
    <<"image/svg+xml">>;
mime_type(<<".png">>) ->
    <<"image/png">>;
mime_type(<<".pdf">>) ->
    <<"application/pdf">>;
mime_type(<<".jpeg">>) ->
    <<"image/jpeg">>;
mime_type(<<".jpg">>) ->
    <<"image/jpeg">>;
mime_type(<<".gif">>) ->
    <<"image/gif">>;
mime_type(<<".weba">>) ->
    <<"audio/webm">>;
mime_type(<<".webm">>) ->
    <<"video/webm">>;
mime_type(<<".webp">>) ->
    <<"image/webp">>;
mime_type(<<".json">>) ->
    <<"application/json">>;
mime_type(<<".xml">>) ->
    <<"application/xml">>;
mime_type(<<".htm">>) ->
    <<"text/html">>;
mime_type(<<".aac">>) ->
    <<"audio/aac">>;
mime_type(<<".abw">>) ->
    <<"application/x-abiword">>;
mime_type(<<".arc">>) ->
    <<"application/x-freearc">>;
mime_type(<<".avif">>) ->
    <<"image/avif">>;
mime_type(<<".avi">>) ->
    <<"video/x-msvideo">>;
mime_type(<<".azw">>) ->
    <<"application/vnd.amazon.ebook">>;
mime_type(<<".bin">>) ->
    <<"application/octet-stream">>;
mime_type(<<".bmp">>) ->
    <<"image/bmp">>;
mime_type(<<".bz">>) ->
    <<"application/x-bzip">>;
mime_type(<<".bz2">>) ->
    <<"application/x-bzip2">>;
mime_type(<<".cda">>) ->
    <<"application/x-cdf">>;
mime_type(<<".csh">>) ->
    <<"application/x-csh">>;
mime_type(<<".csv">>) ->
    <<"text/csv">>;
mime_type(<<".doc">>) ->
    <<"application/msword">>;
mime_type(<<".docx">>) ->
    <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>;
mime_type(<<".eot">>) ->
    <<"application/vnd.ms-fontobject">>;
mime_type(<<".epub">>) ->
    <<"application/epub+zip">>;
mime_type(<<".gz">>) ->
    <<"application/gzip">>;
mime_type(<<".ico">>) ->
    <<"image/vnd.microsoft.icon">>;
mime_type(<<".ics">>) ->
    <<"text/calendar">>;
mime_type(<<".jar">>) ->
    <<"application/java-archive">>;
mime_type(<<".jsonld">>) ->
    <<"application/ld+json">>;
mime_type(<<".mid">>) ->
    <<"audio/midi">>;
mime_type(<<".midi">>) ->
    <<"audio/midi">>;
mime_type(<<".mjs">>) ->
    <<"text/javascript">>;
mime_type(<<".mp3">>) ->
    <<"audio/mpeg">>;
mime_type(<<".mp4">>) ->
    <<"video/mp4">>;
mime_type(<<".mpeg">>) ->
    <<"video/mpeg">>;
mime_type(<<".mpkg">>) ->
    <<"application/vnd.apple.installer+xml">>;
mime_type(<<".odp">>) ->
    <<"application/vnd.oasis.opendocument.presentation">>;
mime_type(<<".ods">>) ->
    <<"application/vnd.oasis.opendocument.spreadsheet">>;
mime_type(<<".odt">>) ->
    <<"application/vnd.oasis.opendocument.text">>;
mime_type(<<".oga">>) ->
    <<"audio/ogg">>;
mime_type(<<".ogv">>) ->
    <<"video/ogg">>;
mime_type(<<".ogx">>) ->
    <<"application/ogg">>;
mime_type(<<".opus">>) ->
    <<"audio/opus">>;
mime_type(<<".otf">>) ->
    <<"font/otf">>;
mime_type(<<".php">>) ->
    <<"application/x-httpd-php">>;
mime_type(<<".ppt">>) ->
    <<"application/vnd.ms-powerpoint">>;
mime_type(<<".pptx">>) ->
    <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>;
mime_type(<<".rar">>) ->
    <<"application/vnd.rar">>;
mime_type(<<".rtf">>) ->
    <<"application/rtf">>;
mime_type(<<".sh">>) ->
    <<"application/x-sh">>;
mime_type(<<".tar">>) ->
    <<"application/x-tar">>;
mime_type(<<".tif">>) ->
    <<"image/tiff">>;
mime_type(<<".tiff">>) ->
    <<"image/tiff">>;
mime_type(<<".ts">>) ->
    <<"video/mp2t">>;
mime_type(<<".txt">>) ->
    <<"text/plain">>;
mime_type(<<".vsd">>) ->
    <<"application/vnd.visio">>;
mime_type(<<".wav">>) ->
    <<"audio/wav">>;
mime_type(<<".woff">>) ->
    <<"font/woff">>;
mime_type(<<".woff2">>) ->
    <<"font/woff2">>;
mime_type(<<".xhtml">>) ->
    <<"application/xhtml+xml">>;
mime_type(<<".xls">>) ->
    <<"application/vnd.ms-excel">>;
mime_type(<<".xlsx">>) ->
    <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>;
mime_type(<<".xul">>) ->
    <<"application/vnd.mozilla.xul+xml">>;
mime_type(<<".zip">>) ->
    <<"application/zip">>;
mime_type(<<".3gp">>) ->
    <<"video/3gpp; audio/3gpp">>;
mime_type(<<".3g2">>) ->
    <<"video/3gpp2; audio/3gpp2">>;
mime_type(<<".7z">>) ->
    <<"application/x-7z-compressed">>;
mime_type(_Unknown) ->
    <<"application/octet-stream">>.
