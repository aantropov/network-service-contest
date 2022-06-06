#include <string.h>
#include <erl_nif.h>

float dot(float* v1, float* v2, size_t len)
{
	float result = 0;
	for (size_t i = 0; i < len; i++)
	{
		result += (*v1) * (*v2);
		v1++;
		v2++;
	}

	return result;
}

static ERL_NIF_TERM dot_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary v1;
	if (!enif_inspect_binary(env, argv[0], &v1))
		return enif_make_badarg(env);

	ErlNifBinary v2;
	if (!enif_inspect_binary(env, argv[1], &v2))
		return enif_make_badarg(env);

	if (v1.size != v2.size)
		return enif_make_badarg(env);

	float result = dot((float*)v1.data, (float*)v2.data, v1.size / 4);
	return enif_make_double(env, (double)result);
}

static ErlNifFunc nif_funcs[] = {
	{"dot", 2, dot_nif}
};

int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

void unload(ErlNifEnv* caller_env, void* priv_data)
{
}

ERL_NIF_INIT(matrix_lib, nif_funcs, load, NULL, upgrade, unload)