package de.innovationgate.testclasses;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.scheduler.ConfigurationException;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.scheduler.Task;
import de.innovationgate.wgpublisher.scheduler.TaskException;
import de.innovationgate.wgpublisher.scheduler.TaskImplementation;

public class TestTask implements TaskImplementation {

    @Override
    public void execute(JobContext jobContext) throws JobFailedException {
        try {
            Thread.sleep(3000);
            App app = WGA.get(jobContext).app(jobContext.getOption("db"));
            app.db().setAttribute("TestTaskRan", true);
            app.db().setAttribute("TestTaskConfigRan", true);
        }
        catch (Exception e) {
            throw new JobFailedException("Exception running TestTask", e);
        }
    }

}
