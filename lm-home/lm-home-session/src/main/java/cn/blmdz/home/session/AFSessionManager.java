package cn.blmdz.home.session;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.home.session.AFSession;
import cn.blmdz.home.session.SessionDataSource;
import cn.blmdz.home.session.redis.SessionRedisSource;
import cn.blmdz.home.session.util.Configuration;
import cn.blmdz.home.session.util.DefaultSessionIdGenerator;
import cn.blmdz.home.session.util.SessionIdGenerator;

public final class AFSessionManager {
	private static final Logger log = LoggerFactory.getLogger(AFSessionManager.class);
	private SessionDataSource dataSource;
	private final SessionIdGenerator sessionIdGenerator;
	private static Configuration configuration = null;

	private AFSessionManager(Configuration configuration) {
		this.sessionIdGenerator = new DefaultSessionIdGenerator();
		if ("redis".equals(configuration.getSource())) {
			if (!configuration.isCluster().booleanValue() && configuration.getSessionRedisHost() == null) {
				throw new IllegalStateException(
						"You have chosen redis source, but not set [session.redis.host] at session.properties");
			} else if (!configuration.isCluster().booleanValue() && configuration.getSessionRedisPort() == null) {
				throw new IllegalStateException(
						"You have chosen redis source, but not set [session.redis.port] at session.properties");
			} else {
				this.dataSource = new SessionRedisSource(configuration);
			}
		} else {
			throw new IllegalStateException(
					"Constructor error: not supported source type : " + configuration.getSource());
		}
	}

	public static AFSessionManager newInstance(Configuration configuration) {
		AFSessionManager.configuration = configuration;
		return AFSessionManager.SingletonHolder.INSTANCE;
	}

	public static AFSessionManager getInstance() {
		return AFSessionManager.SingletonHolder.INSTANCE;
	}

	public Map<String, Object> findSessionById(String prefix, String id) {
		return this.dataSource.findSessionById(prefix, id);
	}

	public void refreshExpireTime(AFSession afSession, int maxInactiveInterval) {
		this.dataSource.refreshExpireTime(afSession, maxInactiveInterval);
	}

	public void deletePhysically(String prefix, String id) {
		this.dataSource.deletePhysically(prefix, id);
	}

	public boolean save(String prefix, String id, Map<String, Object> snapshot, int maxInactiveInterval) {
		return this.dataSource.save(prefix, id, snapshot, maxInactiveInterval);
	}

	public void destroy() {
		this.dataSource.destroy();
	}

	public SessionIdGenerator getSessionIdGenerator() {
		return this.sessionIdGenerator;
	}

	public Configuration getConfiguration() {
		return configuration;
	}

	private static final class SingletonHolder {
		private static final AFSessionManager INSTANCE = init();

		private static AFSessionManager init() {
			return new AFSessionManager(AFSessionManager.configuration);
		}

		public static AFSessionManager getInstance() {
			return INSTANCE;
		}
	}
}
