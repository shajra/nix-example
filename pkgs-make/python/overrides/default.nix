nixpkgs: self: super:

if super.isPy3k
then
    rec {
        # ML packages
        annoy = (import ./annoy) nixpkgs self super;
        backoff = (import ./backoff) nixpkgs self super;
        bokeh = (import ./bokeh) nixpkgs self super;
        editdistance = (import ./editdistance) nixpkgs self super;
        en_core_web_md = (import ./en_core_web_md) nixpkgs self super;
        gensim = (import ./gensim) nixpkgs self super;
        geographiclib = (import ./geographiclib) nixpkgs self super;
        geopy = (import ./geopy) nixpkgs self super;
        implicit = (import ./implicit) nixpkgs self super;
        lda = (import ./lda) nixpkgs self super;
        lime = (import ./lime) nixpkgs self super;
        marshmallow-oneofschema = (import ./marshmallow-oneofschema) nixpkgs self super;
        ortools = (import ./ortools) nixpkgs self super;
        pyldavis = (import ./pyldavis) nixpkgs self super;
        python-igraph = (import ./python-igraph) nixpkgs self super;
        pywavelets = (import ./pywavelets) nixpkgs self super;
        scikit-image = (import ./scikit-image) nixpkgs self super;
        scikitimage = scikit-image;
        scikit-learn = (import ./scikit-learn) nixpkgs self super;
        scikitlearn = scikit-learn;
        scikit-multilearn = (import ./scikit-multilearn) nixpkgs self super;
        spacy = (import ./spacy) nixpkgs self super;
        thinc = (import ./thinc) nixpkgs self super;
        torchfile = (import ./torchfile) nixpkgs self super;
        torchtext = (import ./torchtext) nixpkgs self super;
        visdom = (import ./visdom) nixpkgs self super;

        # Engineering
        avro = (import ./avro) nixpkgs self super;
        cx_oracle = (import ./cx_oracle) nixpkgs self super;
        dill = (import ./dill) nixpkgs self super;
        diskcache = (import ./diskcache) nixpkgs self super;
        hexdump = (import ./hexdump) nixpkgs self super;
        liac-arff = (import ./liac-arff) nixpkgs self super;
        mocket = (import ./mocket) nixpkgs self super;
        pathlib = (import ./pathlib) nixpkgs self super;
        psycopg2 = (import ./psycopg2) nixpkgs self super;
        pyjwt = (import ./pyjwt) nixpkgs self super;
        python-dotenv = (import ./python-dotenv) nixpkgs self super;
        smart_open = (import ./smart_open) nixpkgs self super;
    }
else
    {}
