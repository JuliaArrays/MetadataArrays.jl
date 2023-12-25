using Aqua
using ArrayInterface
using MetadataArrays
using Test

Aqua.test_all(MetadataArrays)

a = [1 2; 3 4; 5 4]
md = (m1 =1, annotation="hello world");
mda = MetadataArray(a, md);
vmda = view(MetadataArray(a, md), :, :);

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
@test mda == vmda == a

@test metadata(mda, :m1) == 1
@test metadata(mda, :m10, 10) == 10
@test MetadataArrays.metadatasupport(typeof(mda)).read
@test all(mda .== a)
@test all(mda .== mda)
@test metadata(mda[:, 1], :annotation) == metadata(vmda[:, 1], :annotation) == md.annotation
@test mda[1, 1] == a[1, 1]


# @test MetadataArrays.can_change_size(MetadataVector{Int,Vector{Int},Dict{Symbol,Any}})
@test ArrayInterface.can_setindex(MetadataVector{Int, Vector{Int}})
@test ArrayInterface.is_forwarding_wrapper(MetadataVector{Int,Vector{Int}})

