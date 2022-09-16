using Documenter, MetadataArrays

DocMeta.setdocmeta!(MetadataArrays, :DocTestSetup, :(using MetadataArrays); recursive=true)

makedocs(
    # options
    modules = [MetadataArrays],
    sitename = "MetadataArrays.jl",
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", "false") == "true",
    ),
    pages = Any[
        "Introduction" => "index.md",
        "API" => "api.md",
    ],
    strict = true,
)

# Deploy built documentation from Travis.
# =======================================

deploydocs(
    # options
    repo = "github.com/JuliaArrays/MetadataArrays.jl.git",
    push_preview = true,
)
