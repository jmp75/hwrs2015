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

        public static double[] ParallelRun(IModelSimulation simulation, int numSystems, int reps)
        {
            var systems = new List<IModelSimulation>();
            for (int i = 0; i < numSystems; i++)
            {
                systems.Add(simulation.CloneModel());
            }
            var parallelOptions = new ParallelOptions() { MaxDegreeOfParallelism = -1 };

            double[] res = new double[reps];
            var sw = new Stopwatch();
            for (int rep = 0; rep < reps; rep++)
            {
                sw.Start();
                Parallel.ForEach(systems, parallelOptions, s => s.Execute());
                sw.Stop();
                res[rep] = sw.Elapsed.TotalMilliseconds;
                sw.Reset();
            }
            return res;
        }
    }
}
