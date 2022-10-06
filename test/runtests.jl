using Aqua
using MetadataArrays
using Test

Aqua.test_all(MetadataArrays)

a = [1 2; 3 4; 5 4]
md = (m1 =1, annotation="hello world");
mda = MetadataArray(a, md);

@test first(mda) == first(a)
@test last(mda) == last(a)
@test size(mda) == size(a)
@test axes(mda) == axes(a)
@test strides(mda) == strides(a)
@test length(mda) == length(a)
@test firstindex(mda) == firstindex(a)
@test lastindex(mda) == lastindex(a)
@test in(1, mda)
@test keys(mda) == keys(a)
@test !isempty(mda)
@test Base.dataids(mda) == Base.dataids(a)
@test IndexStyle(mda) == IndexStyle(a)

@test getproperty(mda, "m1") == 1
@test getproperty(mda, :m1) == 1
@test metadata(mda, "m1") == 1
@test metadata(mda, :m1) == 1
@test metadata(mda, "m10", 10) == 10
@test metadata(mda, :m10, 10) == 10
@test MetadataArrays.metadatasupport(typeof(mda)).read
@test hasproperty(mda, "annotation")
@test hasproperty(mda, :annotation)
@test metadatakeys(mda) == propertynames(mda) == keys(md)
@test all(mda .== a)
@test all(mda .== mda)
@test metadata(mda[:,1], :annotation) == md.annotation
