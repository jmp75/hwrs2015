using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Swift.Bindings;

namespace ParallelSwift
{
    public class SwiftRunTests
    {
        public static void ParallelRun(params IModelSimulation[] simulations)
        {
            var parallelOptions = new ParallelOptions() { MaxDegreeOfParallelism = -1 };
            Parallel.ForEach(simulations, parallelOptions,
                s => s.Execute()
            );
        }

        public static double[] ParallelRun(IModelSimulation simulation, int numSystems, int reps, int maxDegreeOfParallelism = -1)
        {
            var systems = new List<IModelSimulation>();
            for (int i = 0; i < numSystems; i++)
            {
                systems.Add(simulation.CloneModel());
            }
            var parallelOptions = new ParallelOptions() { MaxDegreeOfParallelism = maxDegreeOfParallelism };

            double[] res = new double[reps];
            var sw = new Stopwatch();
            for (int rep = 0; rep < reps; rep++)
            {
                sw.Start();
                if (systems.Count() > 1)
                    Parallel.ForEach(systems, parallelOptions, s => s.Execute());
                else
                    systems[0].Execute();
                sw.Stop();
                res[rep] = sw.Elapsed.TotalMilliseconds;
                sw.Reset();
            }
            return res;
        }
    }
}
