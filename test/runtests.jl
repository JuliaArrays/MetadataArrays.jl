using MetadataArrays, Test

@testset "MetadataVector" begin
    v = [1, 3, 5, 4]
    sv = MetadataVector(v, "Numbers")
    @test length(sv) == 4
    @test collect(sv) == parent(sv) == copy(sv)
    @test metadata(sv) == "Numbers"
    s_view1 = view(sv, 1:3)
    s_view2 = view(s_view1, 1:2)
    @test metadata(s_view1) == metadata(s_view2) == "Numbers"
    sv[3] = 50
    @test all(sv .== [1, 3, 50, 4])
    @test eltype(sv) == Int
    @test metadata(similar(sv)) == metadata(sv)
end

@testset "MetadataArray" begin
    v = [1 2; 3 4; 5 4]
    sv = MetadataArray(v, Dict(1 => "Not at all", 10 => "A lot"))
    @test size(sv) == size(v)
    @test sv[6] == parent(sv)[6]
    @test collect(sv) == parent(sv) == v == copy(sv)
    @test metadata(sv) == Dict(1 => "Not at all", 10 => "A lot")
    sv[3, 2] = 50
    @test all(sv .== [1 2; 3 4; 5 50])
    @test parent(sv[1:2, 1]) == parent(sv)[1:2, 1]
    @test metadata(sv[1:2, 1]) == metadata(sv)
    @test metadata(rand(2)) === nothing
    @test eltype(sv) == Int

    s = MetadataArray(rand(4), "test")
    sr = reshape(s, 2, 2)
    @test parent(sr) == reshape(parent(s), 2, 2)
    @test metadata(sr) == metadata(s)
    @test axes(sr) == (Base.OneTo(2), Base.OneTo(2))
    s2 = similar(s, (2, 3))
    @test size(s2) == (2, 3)
    @test metadata(s2) == metadata(s)

    x1 = rand(4, 4)
    m1 = MetadataArray(x1, "something")
    @test IndexStyle(m1) == IndexLinear()
    x2 = view(x1, 1:2, 1:2)
    m2 = MetadataArray(x2, "something")
    @test IndexStyle(m2) == IndexCartesian()
end
