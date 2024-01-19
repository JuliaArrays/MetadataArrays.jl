module MetadataArraysAdapt

using MetadataArrays
using Adapt

function Adapt.adapt_structure(to, mda::MetadataArray)
    MetadataArrays._MetadataArray(adapt(to, getfield(mda, :parent)), getfield(mda, :metadata))
end

end
