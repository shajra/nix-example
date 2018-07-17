from setuptools import setup, find_packages


setup(
    name="example-python-app",
    packages=find_packages(),
    entry_points={
        'console_scripts': [
            'example-python = example_app.__main__:main',
        ]
    }
)
