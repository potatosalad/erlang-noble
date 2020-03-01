defmodule Noble.MixProject do
  use Mix.Project

  def project() do
    [
      app: :noble,
      version: "0.0.3",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_env: %{"MIX_ENV" => to_string(Mix.env())},
      make_clean: ["clean"],
      make_cwd: "priv",
      name: "Noble",
      source_url: "https://github.com/potatosalad/erlang-noble",
      docs: fn ->
        {ref, 0} = System.cmd("git", ["rev-parse", "--verify", "--quiet", "HEAD"])
        [source_ref: ref, main: "readme", extras: ["README.md", "CHANGELOG.md"]]
      end,
      description: description(),
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application() do
    [
      mod: {:noble_app, []},
      applications: [:kernel, :stdlib, :jason, :json_xema],
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:elixir_make, "~> 0.6", runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:jason, "~> 1.1"},
      {:json_xema, "~> 0.4"}
    ]
  end

  defp description() do
    "A Node.js BLE (Bluetooth Low Energy) port for Erlang and Elixir."
  end

  defp package() do
    [
      maintainers: ["Andrew Bennett"],
      files: [
        "CHANGELOG*",
        "LICENSE*",
        "priv/install.sh",
        "priv/Makefile",
        "priv/noble-ipc-server-stdio.sh",
        "priv/package-lock.json",
        "priv/package.json",
        "priv/yarn.lock",
        "mix.exs",
        "README*",
        "rebar.config",
        "src"
      ],
      licenses: ["MIT"],
      links: %{"Github" => "https://github.com/potatosalad/erlang-noble", "Docs" => "https://hexdocs.pm/noble"}
    ]
  end
end
