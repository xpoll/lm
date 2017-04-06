package io.terminus.galaxy.web.core.component;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.galaxy.common.enums.ZkMsg;
import io.terminus.zookeeper.pubsub.Publisher;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author Effet
 */
@Slf4j
@Component
public class UserCacheCleaner {

    @Autowired(required = false)
    private Publisher zkPublisher;

    private final ExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

    public void clean(Long... userIds) {
        clean(Lists.newArrayList(userIds));
    }

    public void clean(List<Long> userIds) {
        for (Long userId : userIds) {
            executorService.submit(new ClearUserJob(userId));
        }
    }

    class ClearUserJob implements Runnable {

        private final Long userId;

        ClearUserJob(Long userId) {
            this.userId = userId;
        }

        @Override
        public void run() {
            String key = ZkMsg.CLEAR_USER_CACHE + ":" + userId;
            try {
                zkPublisher.publish(key.getBytes());
            } catch (Exception e) {
                log.warn("publish clear user cache command failed, userId={} cause:{}",
                        userId, Throwables.getStackTraceAsString(e));
            }
        }
    }
}
