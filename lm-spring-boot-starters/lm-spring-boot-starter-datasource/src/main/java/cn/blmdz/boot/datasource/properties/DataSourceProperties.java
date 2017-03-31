package cn.blmdz.boot.datasource.properties;

import java.nio.charset.Charset;
import java.util.LinkedHashMap;
import java.util.Map;

import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.EnvironmentAware;
import org.springframework.core.env.Environment;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import cn.blmdz.boot.datasource.autoconfigure.DatabaseDriver;
import cn.blmdz.boot.datasource.autoconfigure.EmbeddedDatabaseConnection;

@ConfigurationProperties(prefix = "datasource")
public class DataSourceProperties implements BeanClassLoaderAware,
		EnvironmentAware, InitializingBean {
	public static final String PREFIX = "datasource";
	private ClassLoader classLoader;
	private Environment environment;
	private String name = "testdb";
	private Class type;
	private String driverClassName;
	private String url;
	private String username;
	private String password;
	private String jndiName;
	private boolean initialize = true;
	private String platform = "all";
	private String schema;
	private String data;
	private boolean continueOnError = false;
	private String separator = ";";
	private Charset sqlScriptEncoding;
	private EmbeddedDatabaseConnection embeddedDatabaseConnection = EmbeddedDatabaseConnection.NONE;
	private DataSourceProperties.Xa xa = new DataSourceProperties.Xa();

	public void setBeanClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	public void afterPropertiesSet() throws Exception {
		this.embeddedDatabaseConnection = EmbeddedDatabaseConnection
				.get(this.classLoader);
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Class getType() {
		return this.type;
	}

	public void setType(Class type) {
		this.type = type;
	}

	public String getDriverClassName() {
		if (StringUtils.hasText(this.driverClassName)) {
			Assert.state(this.driverClassIsLoadable(),
					"Cannot load driver class: " + this.driverClassName);
			return this.driverClassName;
		} else {
			String driverClassName = null;
			if (StringUtils.hasText(this.url)) {
				driverClassName = DatabaseDriver.fromJdbcUrl(this.url)
						.getDriverClassName();
			}

			if (!StringUtils.hasText(driverClassName)) {
				driverClassName = this.embeddedDatabaseConnection
						.getDriverClassName();
			}

			if (!StringUtils.hasText(driverClassName)) {
				throw new DataSourceProperties.DataSourceBeanCreationException(
						this.embeddedDatabaseConnection, this.environment,
						"driver class");
			} else {
				return driverClassName;
			}
		}
	}

	private boolean driverClassIsLoadable() {
		try {
			ClassUtils.forName(this.driverClassName, (ClassLoader) null);
			return true;
		} catch (UnsupportedClassVersionError var2) {
			throw var2;
		} catch (Throwable var3) {
			return false;
		}
	}

	public void setDriverClassName(String driverClassName) {
		this.driverClassName = driverClassName;
	}

	public String getUrl() {
		if (StringUtils.hasText(this.url)) {
			return this.url;
		} else {
			String url = this.embeddedDatabaseConnection.getUrl(this.name);
			if (!StringUtils.hasText(url)) {
				throw new DataSourceProperties.DataSourceBeanCreationException(
						this.embeddedDatabaseConnection, this.environment,
						"url");
			} else {
				return url;
			}
		}
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getUsername() {
		return StringUtils.hasText(this.username) ? this.username
				: (EmbeddedDatabaseConnection.isEmbedded(this
						.getDriverClassName()) ? "sa" : null);
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return StringUtils.hasText(this.password) ? this.password
				: (EmbeddedDatabaseConnection.isEmbedded(this
						.getDriverClassName()) ? "" : null);
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getJndiName() {
		return this.jndiName;
	}

	public void setJndiName(String jndiName) {
		this.jndiName = jndiName;
	}

	public boolean isInitialize() {
		return this.initialize;
	}

	public void setInitialize(boolean initialize) {
		this.initialize = initialize;
	}

	public String getPlatform() {
		return this.platform;
	}

	public void setPlatform(String platform) {
		this.platform = platform;
	}

	public String getSchema() {
		return this.schema;
	}

	public void setSchema(String schema) {
		this.schema = schema;
	}

	public String getData() {
		return this.data;
	}

	public void setData(String script) {
		this.data = script;
	}

	public boolean isContinueOnError() {
		return this.continueOnError;
	}

	public void setContinueOnError(boolean continueOnError) {
		this.continueOnError = continueOnError;
	}

	public String getSeparator() {
		return this.separator;
	}

	public void setSeparator(String separator) {
		this.separator = separator;
	}

	public Charset getSqlScriptEncoding() {
		return this.sqlScriptEncoding;
	}

	public void setSqlScriptEncoding(Charset sqlScriptEncoding) {
		this.sqlScriptEncoding = sqlScriptEncoding;
	}

	public ClassLoader getClassLoader() {
		return this.classLoader;
	}

	public DataSourceProperties.Xa getXa() {
		return this.xa;
	}

	public void setXa(DataSourceProperties.Xa xa) {
		this.xa = xa;
	}

	private static class DataSourceBeanCreationException extends
			BeanCreationException {
		DataSourceBeanCreationException(EmbeddedDatabaseConnection connection,
				Environment environment, String property) {
			super(getMessage(connection, environment, property));
		}

		private static String getMessage(EmbeddedDatabaseConnection connection,
				Environment environment, String property) {
			StringBuilder message = new StringBuilder();
			message.append("Cannot determine embedded database " + property
					+ " for database type " + connection + ". ");
			message.append("If you want an embedded database please put a supported one on the classpath. ");
			message.append("If you have database settings to be loaded from a particular profile you may need to active it");
			if (environment != null) {
				String[] profiles = environment.getActiveProfiles();
				if (ObjectUtils.isEmpty(profiles)) {
					message.append(" (no profiles are currently active)");
				} else {
					message.append(" (the profiles \""
							+ StringUtils
									.arrayToCommaDelimitedString(environment
											.getActiveProfiles())
							+ "\" are currently active)");
				}
			}

			message.append(".");
			return message.toString();
		}
	}

	public static class Xa {
		private String dataSourceClassName;
		private Map properties = new LinkedHashMap();

		public String getDataSourceClassName() {
			return this.dataSourceClassName;
		}

		public void setDataSourceClassName(String dataSourceClassName) {
			this.dataSourceClassName = dataSourceClassName;
		}

		public Map getProperties() {
			return this.properties;
		}

		public void setProperties(Map properties) {
			this.properties = properties;
		}
	}
}
