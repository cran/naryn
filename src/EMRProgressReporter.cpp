#include <sys/time.h>

#include "EMR.h"
#include "EMRProgressReporter.h"

void EMRProgressReporter::init(uint64_t maxsteps, uint64_t init_report_step, uint64_t report_interval, uint64_t min_report_interval)
{
	m_maxsteps = maxsteps;
	m_report_step = init_report_step;
	m_report_interval = report_interval;
	m_min_report_interval = min_report_interval;

	m_numsteps = 0;
	m_numsteps_from_last_report = 0;
	m_last_progress_reported = -1;
	m_last_report_clock = get_cur_clock();
	m_elapsed_clock = 0;
}

uint64_t EMRProgressReporter::get_cur_clock()
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

void EMRProgressReporter::report(uint64_t delta_steps_done)
{
	m_numsteps_from_last_report += delta_steps_done;
	m_numsteps += delta_steps_done;
	if (m_numsteps_from_last_report > m_report_step) {
		uint64_t curclock = get_cur_clock();
		double delta = curclock - m_last_report_clock;

		if (delta)
			m_report_step = (int)(m_report_step * (m_report_interval / delta) + .5);
		else
			m_report_step *= 10;

		if (delta > m_min_report_interval) {
            int progress = m_maxsteps ? (int)(100. * m_numsteps / m_maxsteps) : 0;
            progress = min(progress, 100);

            if (m_last_progress_reported < 0 && !m_report_prefix.empty())
                vemsg("%s", m_report_prefix.c_str());

            if (progress != m_last_progress_reported) {
                if (progress == 100)
                    vemsg("%d%%", progress);
                else
                    vemsg("%d%%...", progress);
            } else
                vemsg(".");
			m_last_progress_reported = progress;
			m_numsteps_from_last_report = 0;
			m_last_report_clock = curclock;
			m_elapsed_clock = (uint64_t)delta;
		}
	}
}

void EMRProgressReporter::report_last()
{
	if (m_last_progress_reported >= 0) {
		if (m_last_progress_reported != 100)
			vemsg("100%%\n");
		else
			vemsg("\n");
	}
}
